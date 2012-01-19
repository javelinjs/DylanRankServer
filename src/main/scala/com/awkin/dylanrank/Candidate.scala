package com.awkin.dylanrank

import java.util.Date

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.MongoDB
import com.mongodb.DBCursor

class Candidate(val db: MongoDB, val baseId: BaseId, 
                    val dblimits: Int) {

    val itemColl = db("item")
    val channelColl = db("channel")

    val sortsNTO = DBObject("pubDate"-> -1)
    val sortsOTN = DBObject("pubDate"-> 1)

    val logger: Logger = LoggerFactory.getLogger(classOf[Candidate])

    def naiveStry: List[DBObject] = {
        val q  = DBObject.empty
        /* FIXME */
        val fields = DBObject("pubDate"->1)

        baseId.exist match {
        case false =>
            itemColl.find(q, fields).sort(sortsNTO).limit(dblimits).toList
        case true => 
            val baseobj: DBObject = 
                itemColl.findOne(
                        MongoDBObject("_id"->baseId.oid)
                ).getOrElse(DBObject.empty)

            /* !! Here throw an exception */
            if (baseobj == DBObject.empty) {
                logger.warn("No such baseid: {}", baseId.sid)
                throw new NoSuchBaseId
            }

            val baseDate = 
                baseobj.getAsOrElse[Date]("pubDate", new Date(0))

            val condSametime = 
                MongoDBObject("pubDate"->baseDate) ++ ("_id" $ne baseId.oid)
            val condOlder = ("pubDate" $lt baseDate)
            val condNewer = ("pubDate" $gt baseDate)

            /* 按比例分配候选item。
                如果是请求新的item，则时间较新的item更有可能被选中，
                因此分配更多的候选名额。反之亦然。 */
            val limitNewTmp = 
                if (baseId.newer) (dblimits*0.8).toInt
                else (dblimits*0.2).toInt
            val limitOld = dblimits - limitNewTmp
            val limitNew = if (limitNewTmp > 0) limitNewTmp else 1
            val limitSametime = (dblimits*0.2).toInt + 1

            val newerItem: MongoCursor = 
                itemColl.find(condNewer, fields).sort(sortsOTN).limit(limitNew)
            /* LOG */
            logger.info("newerItem: " + newerItem.count)
            val olderItem: MongoCursor = 
                itemColl.find(condOlder, fields).sort(sortsNTO).limit(limitOld)
            /* LOG */
            logger.info("olderItem: " + olderItem.count)
            val sametimeItem: MongoCursor = 
                itemColl.find(condSametime, fields).limit(limitSametime)
            /* LOG */
            logger.info("sametimeItem: " + sametimeItem.count)

            (newerItem.toList ++ olderItem.toList ++ 
                            sametimeItem.toList ++ List(baseobj))
        }
    }
}

class NoSuchBaseId extends Exception {}
