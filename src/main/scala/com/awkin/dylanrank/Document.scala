package com.awkin.dylanrank

import scala.actors._
import Actor._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

class Document(val conn: MongoConnection, val maxDocNum: Int) {
    val itemColl = conn(Config.db)("item")
    val channelColl = conn(Config.db)("channel")
    val limits = 
        if (maxDocNum <= 0) 100
        else maxDocNum

    def items(): List[Map[String, Any]] = {
        val q  = DBObject.empty
        val fields = DBObject("title"->1, "channel"->1, 
                                    "desc"->1, "content"->1,
                                    "link"->1, "pubDate"->1)
        val itemCursor = 
            itemColl.find(q, fields).limit(limits)
                    .sort(DBObject("pubDate"-> -1))
        rankByTime(itemCursor)
    }

    /* most recent item get the highest time-rank */ 
    private def rankByTime(itemCursor: MongoCursor): List[Map[String, Any]] = {
        val (itemList, _) = 
            itemCursor.foldLeft((List[Map[String, Any]](), 
                                    itemCursor.count.toFloat)) 
            { (res, itemObj: DBObject) =>
                val (itemList, timerank) = res

                /* get channel info for each item */
                val oid = itemObj.getAs[ObjectId]("channel")
                val channelObj: DBObject = getItemChannel(oid)

                /* combine item, channel and timerank */
                val itemObjWithTimerank: Map[String, Any] = 
                    emitItemData(itemObj, false) ++ 
                        emitChannelData(channelObj) ++ 
                        Map("timerank"->timerank)
                val newList = 
                    itemList ::: List(itemObjWithTimerank)
                (newList, timerank-1)
            }
        itemList
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
