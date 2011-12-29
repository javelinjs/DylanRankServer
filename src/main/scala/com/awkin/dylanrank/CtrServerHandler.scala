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

