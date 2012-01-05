package com.awkin.dylanrank

import scala.actors._
import Actor._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

class Document(val conn: MongoConnection, val setSize: Int, 
                val firstNcontent: Int) {
    val itemColl = conn(Config.db)("item")
    val channelColl = conn(Config.db)("channel")
    val limits = if (setSize <= 0) 50 else setSize
    val limitContent = if (firstNcontent < 0) limits else firstNcontent

    def items(): List[Map[String, Any]] = {
        val q  = DBObject.empty
        val fields = DBObject("title"->1, "channel"->1, 
                                    "desc"->1, "content"->1,
                                    "link"->1, "pubDate"->1)
        val dblimits = limits * 2
        /* get new items */
        val itemCursor = 
            itemColl.find(q, fields).limit(dblimits)
                    .sort(DBObject("pubDate"-> -1))
        /* rank items and sort */
        val sortedList: List[DBObject] = 
            sortItems(rankByTime(itemCursor)).take(limits)
        /* emit data which client need */
        emitData(sortedList)
    }

    private def sortItems(itemList: List[DBObject])
                    : List[DBObject] = {
        itemList.sortWith { (i1: DBObject, i2: DBObject) =>
            i1.getAsOrElse[Float]("rank", -1.0f) >
            i2.getAsOrElse[Float]("rank", -1.0f)
        }
    }

    /* most recent item get the highest time-rank */ 
    private def rankByTime(itemCursor: MongoCursor): List[DBObject] = {
        val (itemList, _) = 
            itemCursor.foldLeft((List[DBObject](), 
                                    itemCursor.count.toFloat)) 
            { (res, itemObj: DBObject) =>
                val (itemList, timerank) = res
                val itemObjWithTimerank: DBObject = 
                    itemObj ++ MongoDBObject("rank"->timerank)
                val newList = 
                    itemList ::: List(itemObjWithTimerank)
                (newList, timerank-1)
            }
        itemList
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
