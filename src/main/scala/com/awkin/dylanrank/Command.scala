package com.awkin.dylanrank

import java.util.Date

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import org.json._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.MongoDB

class Command(val mongoDB: MongoDB) {
    private var command: JSONObject = new JSONObject().put("valid", 0)
    var response: JSONObject = new JSONObject()

    val logger: Logger = LoggerFactory.getLogger(classOf[Ranking])

    val document = new Document(mongoDB)

    def this (cmd: String, db: MongoDB) {
        this(db)
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
            val baseIdStr = itemJson getString "base_id"
            val replyNcontentTmp = itemJson getInt "reply_n_content"

            val replyNcontent = 
                if (replyNcontentTmp < 0) setSize
                else replyNcontentTmp
            val baseId = BaseId(baseIdStr)

            /* get items from db */
            val sortedItems: List[Map[String, Any]] = 
                document.items(setSize, replyNcontentTmp, baseId, userid)

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
                logger.warn(ex.toString)
                responseData.put("success", 0)
                responseData.put("err", "Invalid data format")
            case ex: NoSuchBaseId =>
                logger.warn(ex.toString)
                responseData.put("success", 0)
                responseData.put("err", "No such base_id")
            case exOther =>
                logger.warn(exOther.toString)
                responseData.put("success", 0)
                responseData.put("err", "Unknown")
        }
        responseData
    }

}

class BaseId(val idstr: String) {
    val (exist:Boolean, sid: String, 
            oid:ObjectId, newer:Boolean) = extractBaseId

    private def extractBaseId : (Boolean, String, ObjectId, Boolean) = {
        val (exist, sid, newer) =
            if (idstr.length > 0) {
                idstr(0) match {
                case '-' => (true, idstr.substring(1), true)
                case '+' => (true, idstr.substring(1), false)
                case _ => (true, idstr, false)
                }
            } else {
                (false, "", false)
            }
        val oid = 
            exist match {
            case true =>
                try {
                    new ObjectId(sid)
                } catch {
                    case _ => throw new NoSuchBaseId
                }
            case false =>
                new ObjectId()
            }
        (exist, sid, oid, newer)
    }
}
object BaseId {
    def apply(idstr: String) = {
        new BaseId(idstr)
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
