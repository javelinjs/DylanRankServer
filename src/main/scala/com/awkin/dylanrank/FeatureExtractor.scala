package com.awkin.dylanrank

import org.json._
import java.io._

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.MongoDB

object FeaDesc {
    def feaKeys: Map[String, FeaFetcher] = 
        Map("length_title" -> new IntFeaFetcher("length_title"), 
            "length_content" -> new IntFeaFetcher("length_content"), 
            "length_desc" -> new IntFeaFetcher("length_desc"), 
            "source" -> new StringFeaFetcher("source"),
            "uid" -> new StringFeaFetcher("uid"),
            "oid" -> new StringFeaFetcher("oid"))
}

abstract class FeaFetcher(val key: String) {
    def fetchFeature(obj: MongoDBObject): Double
}
class IntFeaFetcher(key: String) extends FeaFetcher(key) {
    override def fetchFeature(obj: MongoDBObject): Double = {
        obj.getAsOrElse[Int](key, 0).toDouble
    }
}
class StringFeaFetcher(key: String) extends FeaFetcher(key) {
    override def fetchFeature(obj: MongoDBObject): Double = {
        obj.getAsOrElse[String](key, "").toLowerCase.hashCode.toDouble
    }
}

class FeaExtractor(val db: MongoDB) {
    import FeatureType._
    val feaItemColl = db("feature")
    /*
    val itemColl = db("item")
    val channelColl = db("channel") */
    val logger: Logger = LoggerFactory.getLogger(classOf[FeaExtractor])

    /* get features of a specific item from db. */
    def getFeature(userid: String, itemid: String): Map[FeatureType, Double] = {
        /* check whether id are valid */
        val (uid: ObjectId, uidValid: Boolean) = 
            try {
                (new ObjectId(userid), true)
            } catch {
                case _ => (new ObjectId(), false)
            }
        val (oid: ObjectId, oidValid: Boolean) = 
            try {
                (new ObjectId(itemid), true)
            } catch {
                case _ => (new ObjectId(), false)
            }

        /* get features from db */
        val feaItemObjOption = 
            if (oidValid) {
                //val field = DBObject("title"->1, "channel"->1)
                val field = DBObject.empty
                val fea = feaItemColl.findOne(MongoDBObject("item"->oid), field)

                Some(
                    fea.getOrElse(MongoDBObject()) ++ 
                        MongoDBObject("uid"->userid) ++ 
                        MongoDBObject("oid"->itemid)
                )
            } else {
                None
            }
        /* contruct the feature list 
         * Map() if nothing found 
         * Actually at most one feaItem could be found
         */
        (Map[FeatureType, Double]() /: feaItemObjOption) { (map, feaItemObj) =>
            map ++
            /* get feature according to the FeaDesc setting */
            (Map[FeatureType, Double]() /: FeaDesc.feaKeys.keys) { (feaMap, key) =>
                val value:Double = FeaDesc.feaKeys(key).fetchFeature(feaItemObj)
                feaMap + (BeFeatureType("["+key+"]") -> value)
            }
        }
    }
}

