package com.awkin.dylanrank

import java.util.Date
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoDB

class Ranking(val itemList: List[DBObject], val db: MongoDB) {
    /* most recent item get the highest time-rank */ 
    def rank(): List[DBObject] = {
        /*
        val sortedList = itemList.sortWith { 
            (i1: DBObject, i2: DBObject) =>
                i1.getAsOrElse[Date]("pubDate", new Date(0)).after(
                    i2.getAsOrElse[Date]("pubDate", new Date(0)))
        }*/
        val (resList, _) = 
        ((List[DBObject](), itemList.length.toFloat) /: itemList) 
        { (res, itemObj: DBObject) =>
            val (list, timerank) = res
            val itemObjWithTimerank: DBObject = 
                itemObj ++ MongoDBObject("rank"->timerank)
            val newList = 
                list ::: List(itemObjWithTimerank)
            (newList, timerank-1)
        }
        resList
    }
}
