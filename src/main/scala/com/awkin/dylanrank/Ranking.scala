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
    /* most recent item get the highest time-rank */ 
    def rank(items: List[DBObject], userid: String): List[DBObject] = {
        /*
        val itemList = items.sortWith { 
            (i1: DBObject, i2: DBObject) =>
                i1.getAsOrElse[Date]("pubDate", new Date(0)).after(
                    i2.getAsOrElse[Date]("pubDate", new Date(0)))
        }*/
        val (resList, _) = 
        ((List[DBObject](), items.length.toFloat) /: items) 
        { (res, itemObj: DBObject) =>
            val (list, timerank) = res
            //val feas = feaExtractor.getFeature(userid, itemObj._id.toString)
            val feas = feaExtractor.getFeature(userid, itemObj._id.get.toString)
            val weight = ActorObject.modelActor.calculateWeight(feas)
            if (logger.isDebugEnabled) {
                logger.debug("features for item({}) = {}", 
                                itemObj._id.toString, feas.toString)
                logger.debug("weight for item({}) = {}", 
                                itemObj._id.toString, weight)
            }
            val itemObjWithTimerank: DBObject = 
                itemObj ++ MongoDBObject("timerank"->timerank) ++ 
                    MongoDBObject("weight"->weight)
            val newList = 
                list ::: List(itemObjWithTimerank)
            (newList, timerank-1)
        }
        resList
    }
}
