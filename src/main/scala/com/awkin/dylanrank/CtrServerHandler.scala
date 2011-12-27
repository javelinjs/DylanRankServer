package com.awkin.dylanrank

import java.util.Date

import org.apache.mina.core.session.IdleStatus
import org.apache.mina.core.service.IoHandlerAdapter
import org.apache.mina.core.session.IoSession

import org.json._

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

import scala.actors._
import Actor._

class CtrServerHandler(val conn: MongoConnection) extends IoHandlerAdapter {
    /* when exception caught */
    override def exceptionCaught(session: IoSession, cause:Throwable) {
        cause.printStackTrace
        session.close
    }

    /* when receive message from client */
    override def messageReceived(session: IoSession, message:Object) {
        val str = message.toString

        val command = new Command(str, conn)
        command.run

        /* get items from db */
        /*
        val testItem = new Document(conn, 2)
        val items: List[Map[String, Any]] = testItem.items()
        */

        /* sort the items according to the rank */
        /*
        val sortedItems = items.sortWith { (i1, i2) => 
            i1.get("timerank").getOrElse(-1.0).asInstanceOf[Float] >
                i2.get("timerank").getOrElse(-1.0).asInstanceOf[Float]
        }
        */

        /* generate the JSON response */
        /*
        val jsonData = new JSONArray()
        for (item <- sortedItems) {
            val jsonItem = new JSONObject()
            for ((key, value) <- item) { jsonItem.put(key, value) }
            jsonData put jsonItem
        }
        val responseData = new JSONObject().put("success", 1)
        responseData.put("items", jsonData)
        */

        /* response to client */
        session.write(command.response.toString)
        session.close
        println("Message written...");
    }

    /* when new connection established */
    override def sessionOpened(session: IoSession) {
        println("session open for " + session.getRemoteAddress)
    }

    /* when connection closed */
    override def sessionClosed(session: IoSession) {
        println("session closed from " + session.getRemoteAddress)
    }

    override def sessionIdle(session: IoSession, status: IdleStatus) {
        println( "IDLE " + session.getIdleCount(status))
        if (session.getIdleCount(status) > 5) {
            session.close
        }
    }
}

