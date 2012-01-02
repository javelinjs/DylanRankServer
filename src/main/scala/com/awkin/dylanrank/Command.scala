package com.awkin.dylanrank

import java.util.Date

import org.json._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

class Command(val mongoConn: MongoConnection) {
    private var command: JSONObject = new JSONObject().put("valid", 0)

    var response: JSONObject = new JSONObject()

    def this (cmd: String, conn: MongoConnection) {
        this(conn)
        putCmd(cmd)
    }

    def putCmd(cmd: String) = {
        command = 
            try {
                new JSONObject(cmd).put("valid", 1)
            } catch {
                case ex: JSONException => new JSONObject().put("valid", 0)
            }
        this
    }

    def run(): JSONObject = {
        command match {
            case CmdInvalid() => 
                val res: JSONObject = new JSONObject()
                res.put("success", 0)
                res.put("err", "Invalid request format")
                response = res
            case CmdGetItem() =>
                val data = command getJSONObject "data"
                response = cmdGetItem(data)
            case _ => 
                val res: JSONObject = new JSONObject()
                res.put("success", 0)
                res.put("err", "Unknown request")
                response = res
        }
        response
    }

    private def cmdGetItem(jsonData: JSONObject): JSONObject = {
        val responseData = new JSONObject()
        try {
            val userJson = jsonData getJSONObject "user"
            val itemJson = jsonData getJSONObject "item"

            val userid = userJson getString "user_id"
            val setSize = itemJson getInt "set_size"
            val baseId = itemJson getString "base_id"

            /* get items from db */
            val document = new Document(mongoConn, setSize*2)
            val items: List[Map[String, Any]] = document.items()

            /* sort the items according to the rank */
            val sortedItems = items.sortWith { (i1, i2) => 
                i1.get("timerank").getOrElse(-1.0).asInstanceOf[Float] >
                    i2.get("timerank").getOrElse(-1.0).asInstanceOf[Float]
            }.take(setSize)

            /* generate the JSON response */
            val jsonRes = new JSONArray()
            for (item <- sortedItems) {
                val jsonItem = new JSONObject()
                for ((key, value) <- item) { jsonItem.put(key, value) }
                jsonRes put jsonItem
            }
            responseData.put("success", 1)
            responseData.put("size", jsonRes.length)
            responseData.put("items", jsonRes)
        } catch {
            case ex: JSONException => 
                responseData.put("success", 0)
                responseData.put("err", "Invalid data format")
            case _ =>
                responseData.put("success", 0)
                responseData.put("err", "Unknown")
        }
        responseData
    }
}

/* case Objects */
object CmdInvalid {
    def apply(cmd: JSONObject) : Boolean = {
        unapply(cmd)
    }

    def unapply(cmd: JSONObject) : Boolean = {
        try {
            cmd.getInt("valid") == 0
        } catch {
            case _ => false
        }
    }
}

object CmdGetItem {
    def apply(cmd: JSONObject) : Boolean = {
        unapply(cmd)
    }

    def unapply(cmd: JSONObject) : Boolean = {
        try {
            cmd.getString("cmd") == "get_item"
        } catch {
            case _ => false    
        }
    }
}

object ErrCode {
    val errCode = Map("401"->"Invalid request format",
                        "402"->"Invalid data format",
                        "409"->"Unknown")
}
