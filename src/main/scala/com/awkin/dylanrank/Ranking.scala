package com.awkin.dylanrank

import java.util.Date
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoDB

import scala.actors._
import Actor._

class Ranking(val db: MongoDB) {
    val logger: Logger = LoggerFactory.getLogger(classOf[Ranking])

    val feaExtractor = new FeaExtractor(db)

    private val levelNum = 4

    /* if no userid specificed, then
     * most recent item get the highest time-rank 
     */ 
    def rank(items: List[DBObject]): List[DBObject] = {
        val (resList, _) = 
        ((List[DBObject](), items.length.toFloat) /: items) 
        { (res, itemObj: DBObject) =>
            val (list, timerank) = res
            val itemObjWithRank: DBObject = 
                itemObj ++ MongoDBObject("timerank"->timerank) ++ 
                    MongoDBObject("rank"->timerank.toDouble) ++
                    MongoDBObject("level"->0)
            val newList = 
                list ::: List(itemObjWithRank)
            (newList, timerank-1)
        }
        resList
    }
    def rank(items: List[DBObject], userid: String): List[DBObject] = {
        val (resList, _) = 
        ((List[DBObject](), items.length) /: items) 
        { (res, itemObj: DBObject) =>
            val (list, timerank) = res
            val feas = feaExtractor.getFeature(userid, itemObj._id.get.toString)
            //TODO: here id not used
            ActorObject.modelActor ! ("calculate", self, 0, feas)
            val (rank, showlevel) = 
                receiveWithin(1000) {
                case (0, r:Double, stat:Statistics) => (r, 0)  //level not use
                case (_, r:Double, stat:Statistics) => (0.0, 0)
                case TIMEOUT => (0.0, 0)
                }

            if (logger.isDebugEnabled) {
                logger.debug("features for item({}) = {}", 
                                itemObj._id.toString, feas.toString)
                logger.debug("rank for item({}) = {}", 
                                itemObj._id.toString, rank)
                logger.debug("showlevel = {}", showlevel)
            }

            val itemObjWithRank: DBObject = 
                itemObj ++ MongoDBObject("timerank"->timerank) ++ 
                    MongoDBObject("rank"->rank) ++
                    MongoDBObject("level"->showlevel)
            val newList = 
                list ::: List(itemObjWithRank)
            (newList, timerank-1)
        }
        resList
    }

    private def level(r: Double): Int = {
        if (r > 0.85) 3
        else if (r > 0.7) 2
        else if (r > 0.5) 1
        else 0
    }
    private def level(r: Double, stat: Statistics): Int = {
        val rMax = stat.rankMax
        val rMin = stat.rankMin
        val span = (rMax - rMin) / levelNum

        levelHelper(r, rMin, span, 0)
    }
    private def levelHelper(r: Double, rStart: Double, 
                                span: Double, loop: Int): Int = {
        if (r <= rStart || loop == levelNum-1) loop 
        else levelHelper(r, rStart+span, span, loop+1)
    }
}
