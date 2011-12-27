package com.awkin.dylanrank

//import scala.collection.immutable._
import scala.actors._
import Actor._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

//class Document extends Actor {
class Document(val conn: MongoConnection, val maxDocNum: Int) {
    val itemColl = conn(Config.db)("item")
    val channelColl = conn(Config.db)("channel")
    val limits = 
        if (maxDocNum < 0) 100
        else maxDocNum

    def items(): List[Map[String, Any]] = {
        val q  = DBObject.empty
        val fields = DBObject("title"->1, "channel"->1, 
                                    "link"->1, "pubDate"->1)
        val itemCursor = 
            itemColl.find(q, fields).limit(limits)
                    .sort(DBObject("pubDate"-> -1))
        /* get channels belongs to the item 
            most recent item get the highest time-rank */ 
        val (itemList, _) = 
            itemCursor.foldLeft((List[Map[String, Any]](), 
                                    itemCursor.count.toFloat)) 
            { (res, itemObj: DBObject) =>
                val (itemList, timerank) = res

                /* get channel info for each item */
                val oid = itemObj.getAs[ObjectId]("channel")
                val channelObj: DBObject = 
                    channelColl.findOne(
                        MongoDBObject("_id"->oid),
                        DBObject("title"->1, "link"->1, "desc"->1, 
                                "lastBuildDate"->1)
                    ).getOrElse(DBObject.empty)

                /* add channel info and timerank */
                val itemObjWithTimerank: Map[String, Any] = 
                    Map("channel_title"->channelObj.getOrElse("title", ""),
                        "channel_desc"->channelObj.getOrElse("desc", ""),
                        "channel_date"->channelObj.getOrElse("lastBuildDate", ""),
                        "channel_link"->channelObj.getOrElse("link", ""),
                        "timerank"->timerank
                    ) ++ itemObj
                val newList = 
                    itemList ::: List(itemObjWithTimerank)
                (newList, timerank-1)
            }
        itemList
    }
}
