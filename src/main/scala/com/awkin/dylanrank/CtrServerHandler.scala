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
        val END_SIGN = "{:DYLAN:END:}"
        val SUB_COUNT = 5000

        val str = message.toString

        val command = new Command(str, conn)
        command.run

        val res = command.response.toString
        /* response to client */
        val loops = res.length / SUB_COUNT
        for (i <- 0 until loops) {
            session write res.substring(i*SUB_COUNT, (i+1)*SUB_COUNT)
        }
        session write res.substring(loops*SUB_COUNT)
        session write END_SIGN

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

