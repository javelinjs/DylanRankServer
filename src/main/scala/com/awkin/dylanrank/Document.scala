package com.awkin.dylanrank

import java.util.Date
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

import scala.actors._
import Actor._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.mongodb.DBCursor

class Document(val conn: MongoConnection, val setSize: Int, 
                val firstNcontent: Int, val baseId: BaseId) {

    val itemColl = conn(Config.db)("item")
    val channelColl = conn(Config.db)("channel")

    val limits = if (setSize <= 0) 50 else setSize
    val limitContent = if (firstNcontent < 0) limits else firstNcontent

    /* store the temp step result */
    private var resItemList: List[DBObject] = List[DBObject]()

    def items(): List[Map[String, Any]] = {
        val dblimits = limits * 2
        /* get new items */
        getItems(dblimits) 
        /* rank items and sort */
        sortItems(rankByTime(resItemList))
        /* emit data which client need */
        if (baseId.exist) {
            chooseItemAccordBaseId()
        } 
        resItemList = resItemList.take(limits)
        emitData(resItemList)
    }

    private def chooseItemAccordBaseId(): List[DBObject] = {
        val obj = resItemList.find(
                    _.getAsOrElse[String]("_id", "") == baseId.sid)
        val idx = resItemList indexOf obj.getOrElse(DBObject.empty)
        if (idx < 0) {
            println("WARNING: idx < 0")
            //Log sth wrong
        } else {
            baseId.newer match {
            case true =>
                resItemList = resItemList.take(idx)
            case false =>
                val idxR = resItemList.length-idx-1
                resItemList = resItemList.takeRight(idxR)
            }
        }
        resItemList
    }

    private def getItems(dblimits: Int): List[DBObject] = {
        val q  = DBObject.empty
        val fields = DBObject("title"->1, "channel"->1, 
                                    "desc"->1, "content"->1,
                                    "link"->1, "pubDate"->1)
        val sortsNTO = DBObject("pubDate"-> -1)
        val sortsOTN = DBObject("pubDate"-> 1)

        baseId.exist match {
        case false =>
            resItemList = 
                itemColl.find(q, fields).sort(
                            sortsNTO).limit(dblimits).toList
        case true => 
            val obj: DBObject = 
                itemColl.findOne(
                        MongoDBObject("_id"->baseId.oid),
                        DBObject("pubDate"->1)
                ).getOrElse(DBObject.empty)

            /* !! Here throw an exception */
            if (obj == DBObject.empty) {
                println("WARING: no such baseid")
                throw new NoSuchBaseId
            }

            val baseDate = 
                obj.getAsOrElse[Date]("pubDate", new Date(0))

            val condOlder = "pubDate" $lt baseDate
            val condNewer = "pubDate" $gte baseDate

            /* 按比例分配候选item。
                如果是请求新的item，则时间较新的item更有可能被选中，
                因此分配更多的候选名额。反之亦然。 */
            val limitNewTmp = 
                if (baseId.newer) (dblimits*0.8).toInt
                else (dblimits*0.2).toInt
            val limitOld = dblimits - limitNewTmp
            val limitNew = if (limitNewTmp > 0) limitNewTmp else 1

            val newerItem: MongoCursor = 
                itemColl.find(condNewer, fields).sort(sortsOTN).limit(limitNew)
            val olderItem: MongoCursor = 
                itemColl.find(condOlder, fields).sort(sortsNTO).limit(limitOld)
            resItemList = (newerItem.toList ++ olderItem.toList)
        }
        resItemList
    }

    private def sortItems(itemList: List[DBObject]) : List[DBObject] = {
        resItemList = 
            itemList.sortWith { (i1: DBObject, i2: DBObject) =>
                i1.getAsOrElse[Float]("rank", -1.0f) >
                i2.getAsOrElse[Float]("rank", -1.0f)
            }
        resItemList
    }

    private def emitData(dataList: List[DBObject]): List[Map[String, Any]] = {
        val (result, _) =
            ( (List[Map[String, Any]](), 0) /: dataList ) { (res, itemObj) =>
                /* fetch res of last loop */
                val (resList, i) = res

                /* get channel info for each item */
                val oid = itemObj.getAs[ObjectId]("channel")
                val channelObj: DBObject = getItemChannel(oid)

                /* combine item and channel */
                val getContent = if (i < limitContent) true else false
                val data: Map[String, Any] = 
                    emitItemData(itemObj, getContent) ++ 
                        emitChannelData(channelObj)
                (resList ::: List(data), i+1)
            }
        result
    }

    private def emitItemData(obj: DBObject, emitContent: Boolean)
                    : Map[String, Any] = {

        val descTmp = obj.getAsOrElse[String]("desc", "")
        val contentTmp = obj.getAsOrElse[String]("content", "")

        val content =
            if (!emitContent) ""
            else if (contentTmp == "") descTmp
            else contentTmp
        val desc = 
            if (descTmp == "") DocDecorate.cutDesc(contentTmp)
            else if (contentTmp == "")  DocDecorate.cutDesc(descTmp)
            else descTmp

        Map("_id"->obj.getAs[ObjectId]("_id").getOrElse(""),
            "title"->obj.getOrElse("title", ""),
            "pubDate"->obj.getOrElse("pubDate", ""),
            "link"->obj.getOrElse("link", ""),
            "desc"->desc,
            "content"->content
        )
    }

    private def emitChannelData(obj: DBObject): Map[String, Any] = {
        Map("channel_title"->obj.getOrElse("title", ""),
            "channel_desc"->obj.getOrElse("desc", ""),
            "channel_date"->obj.getOrElse("lastBuildDate", ""),
            "channel_link"->obj.getOrElse("link", "")
        ) 
    }

    /* get channels belongs to the item */
    private def getItemChannel(oid: Option[ObjectId]): DBObject = {
        channelColl.findOne(
                MongoDBObject("_id"->oid),
                DBObject("title"->1, "link"->1, "desc"->1, 
                        "lastBuildDate"->1)
        ).getOrElse(DBObject.empty)
    }

    private def getItemContent(oid: Option[ObjectId]): String = {
        val itemObj: DBObject = 
            itemColl.findOne(
                MongoDBObject("_id"->oid),
                DBObject("content"->1)
            ).getOrElse(DBObject.empty)
        itemObj.getAsOrElse[String]("content", "")
    }

    /* need to move to Ranking class */
    /* most recent item get the highest time-rank */ 
    private def rankByTime(itemList: List[DBObject]): List[DBObject] = {
        val sortedList = itemList.sortWith { 
            (i1: DBObject, i2: DBObject) =>
                i1.getAsOrElse[Date]("pubDate", new Date(0)).after(
                    i2.getAsOrElse[Date]("pubDate", new Date(0)))
        }
        //itemCursor.sort(DBObject("pubDate"-> -1)) 
        val (resList, _) = 
            sortedList.foldLeft((List[DBObject](), 
                                    sortedList.length.toFloat)) 
            { (res, itemObj: DBObject) =>
                val (itemList, timerank) = res
                val itemObjWithTimerank: DBObject = 
                    itemObj ++ MongoDBObject("rank"->timerank)
                val newList = 
                    itemList ::: List(itemObjWithTimerank)
                (newList, timerank-1)
            }
        resList
    }
}

object DocDecorate {
    def cutDesc(str: String): String = {
        /*
        val pattern = """<p>.*</p>""".r
        val strMatch = pattern.findFirstIn(str).getOrElse("")
        strMatch.length match {
            case 0 => ""
            case len => strMatch.substring(3, len-4)
        }*/
        try {
            str.split("<p>")(1).split("</p>")(0).trim
        } catch {
            case _ => ""
        }
    }
}

class NoSuchBaseId extends Exception {}
