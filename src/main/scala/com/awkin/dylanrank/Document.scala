package com.awkin.dylanrank

import java.util.Date

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import scala.actors._
import Actor._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoDB
import com.mongodb.DBCursor

class Document(val db: MongoDB) {

    val itemColl = db("item")
    val channelColl = db("channel")

    val candidate = new Candidate(db)
    val ranker = new Ranking(db)

    val logger: Logger = LoggerFactory.getLogger(classOf[Document])

    def items(setSize: Int, firstNcontent: Int, baseId: BaseId, userid: String): 
    List[Map[String, Any]] = {
        val limits = if (setSize <= 0) 50 else setSize
        val limitContent = 
            if (firstNcontent < 0) limits else firstNcontent
        /* get candidates */
        val candidateList = candidate.naiveStry(baseId, limits*3)
        /* rank items and sort */
        val rankedList = 
            if (userid == Config.guestId) {
                ranker.rank(candidateList)
            } else { 
                ranker.rank(candidateList, userid)
            }
        val sortedList = sortItemsByRank(rankedList)

        /* emit data which client need */
        val selectedList = 
            if (baseId.exist) {
                chooseItemAccordBaseId(sortedList, baseId)
            } else {
                sortedList
            }
        /* choose the first [limits] big rank */
        val limitedList = selectedList.take(limits)
        emitData(sortItemsByTime(limitedList), limitContent)
    }

    private def chooseItemAccordBaseId(itemList: List[DBObject], 
                                        baseId: BaseId): 
    List[DBObject] = {
        val obj = itemList.find(
                    _.getAsOrElse[String]("_id", "") == baseId.sid)
        val idx = itemList indexOf obj.getOrElse(DBObject.empty)

        if (logger.isDebugEnabled) {
            logger.debug("chooseItemList: {}", itemList.length)
            logger.debug("idx : {}", idx)
        }

        if (idx < 0) {
            logger.warn("idx < 0, which is {}", idx)
            itemList
        } else {
            baseId.newer match {
            case true =>
                itemList.take(idx)
            case false =>
                val idxR = itemList.length-idx-1
                itemList.takeRight(idxR)
            }
        }
    }


    private def sortItemsByRank(itemList: List[DBObject]) : List[DBObject] = {
        itemList.sortWith { (i1: DBObject, i2: DBObject) =>
            i1.getAsOrElse[Double]("rank", -1.0f) >
            i2.getAsOrElse[Double]("rank", -1.0f)
        }
    }
    private def sortItemsByTime(itemList: List[DBObject]) : List[DBObject] = {
        itemList.sortWith { (i1: DBObject, i2: DBObject) =>
            i1.getAsOrElse[Int]("timerank", -1) >
            i2.getAsOrElse[Int]("timerank", -1)
        }
    }

    private def emitData(dataList: List[DBObject], limitContent: Int): 
    List[Map[String, Any]] = {
        val (result, _) =
            ( (List[Map[String, Any]](), 0) /: dataList ) { (res, itemCandidate) =>
                /* fetch res of last loop */
                val (resList, i) = res

                /* get item info according to the item oid */
                val oidItem = itemCandidate.getAs[ObjectId]("_id")
                val itemObj: DBObject = getItemFromDB(oidItem)

                /* get channel info for each item */
                val oidChannel = itemObj.getAs[ObjectId]("channel")
                val channelObj: DBObject = getChannelFromDB(oidChannel)

                /* combine item and channel */
                val getContent = if (i < limitContent) true else false
                val data: Map[String, Any] = 
                    emitItemData(itemObj, getContent) ++ 
                        emitChannelData(channelObj) ++
                        Map("level" -> 
                            itemCandidate.getAsOrElse[Int]("level", 0))
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
            else DocDecorate.removeTag(descTmp)

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

    private def getItemFromDB(oid: Option[ObjectId]): DBObject = {
        val field = DBObject("title"->1, "channel"->1,
                            "desc"->1, "content"->1, 
                            "link"->1, "pubDate"->1)
        itemColl.findOne(
                MongoDBObject("_id"->oid), field).getOrElse(DBObject.empty)
    }

    private def getChannelFromDB(oid: Option[ObjectId]): DBObject = {
        channelColl.findOne(
                MongoDBObject("_id"->oid),
                DBObject("title"->1, "link"->1, "desc"->1, 
                        "lastBuildDate"->1)
        ).getOrElse(DBObject.empty)
    }
}

object DocDecorate {
    val htmlTagPattern = "(<[^>]*>)".r
    val brPattern = "(/r+|/n+)".r

    def cutDesc(str: String): String = {
        try {
            str.split("<p>")(1).split("</p>")(0).trim
        } catch {
            case _ => ""
        }
    }

    def removeTag(content: String): String = {
        // 去掉<>标签
        val textHtml = htmlTagPattern.replaceAllIn(content, "")
        // 去掉换行或回车符号
        brPattern.replaceAllIn(textHtml, "")
    }
}

