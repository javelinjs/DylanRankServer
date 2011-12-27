package com.awkin.dylanrank

import java.util.Date

import org.json._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

class Command {
    private var command: JSONObject = new JSONObject().put("valid", 0)
    private var mongoConn: MongoConnection

    var response: JSONObject = new JSONObject()

    def this (cmd: String, conn: MongoConnection) {
        putCmd(cmd)
        putMongoConn(conn)
    }

    def putMongoConn(conn: MongoConnection) {
        mongoConn = conn
    }

    def putCmd(cmd: String) = {
        command = 
            try {
                new JSONObject(str).put("valid", 1)
            } catch {
                case ex: JSONException => new JSONObject().put("valid", 0)
            }
    }

    def run(): JSONObject = {
        response = 
        command match {
            case CmdInvalid() => 
                val res = new JSONObject()
                res.put("success", 0)
                res.put("err", "Invalid request format")
                res
            case CmdGetItem() =>
                val data = command getJSONObject "data"
                cmdGetItem(data)
        }
    }

    private def cmdGetItem(jsonData: JSONObject) = {
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
            }

            /* generate the JSON response */
            val jsonData = new JSONArray()
            for (item <- sortedItems) {
                val jsonItem = new JSONObject()
                for ((key, value) <- item) { jsonItem.put(key, value) }
                jsonData put jsonItem
            }
            val responseData = new JSONObject().put("success", 1)
            responseData.put("items", jsonData)
        } catch {
            case ex: JSONException => 
                val responseData = new JSONObject().put("success", 0)
                responseData.put("err", "Invalid data format")
            case _ =>
                val responseData = new JSONObject().put("success", 0)
                responseData.put("err", "Unknown")
        }
    }
}

object CmdInvalid {
    def apply(cmd: JSONObject) : Boolean = {
        unapply(cmd)
    }

    def unapply(cmd: JSONObject) : Boolean = {
        cmd.getInt("valid") == 0
    }
}

object CmdGetItem {
    def apply(cmd: JSONObject) : Boolean = {
        unapply(cmd)
    }

    def unapply(cmd: JSONObject) : Boolean = {
        cmd.getString("cmd") == "get_item"
    }
}
