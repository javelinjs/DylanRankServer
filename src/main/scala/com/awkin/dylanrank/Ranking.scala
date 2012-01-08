package com.awkin.dylanrank

import java.util.Date
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

import com.mongodb.casbah.Imports._

class Ranking(val itemList: List[DBObject]) {
    /* most recent item get the highest time-rank */ 
    def rank(): List[DBObject] = {
        val sortedList = itemList.sortWith { 
            (i1: DBObject, i2: DBObject) =>
                i1.getAsOrElse[Date]("pubDate", new Date(0)).after(
                    i2.getAsOrElse[Date]("pubDate", new Date(0)))
        }
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
