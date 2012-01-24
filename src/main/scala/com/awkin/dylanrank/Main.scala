package com.awkin.dylanrank

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.charset.Charset

import org.apache.mina.core.service.IoAcceptor
import org.apache.mina.core.session.IdleStatus
import org.apache.mina.filter.codec.ProtocolCodecFilter
import org.apache.mina.filter.codec.textline.TextLineCodecFactory
import org.apache.mina.filter.logging.LoggingFilter
import org.apache.mina.transport.socket.nio.NioSocketAcceptor

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

object Main {
    def main(args: Array[String]) {
        val logger = LoggerFactory.getLogger("Main")
        /* first arg is the conf file path */
        val confFile = 
            if (args.length > 0) { 
                Some(args(0))
            } else {
                None
            }
        Config.readConf(confFile)

        val mongoConn = MongoConnection(Config.dbHost, Config.dbPort)
        val mongoDB = mongoConn(Config.db)
        val auth = mongoDB.authenticate(Config.dbUser, Config.dbPwd)
        if (auth == false) {
            logger.error("Error to connect to mongodb")
            return
        }
        /*
        val docService = new Document()
        docService.start() */

        val acceptor: IoAcceptor = new NioSocketAcceptor

        acceptor.getFilterChain().addLast("logger", new LoggingFilter())
        acceptor.getFilterChain().addLast("codec", 
                new ProtocolCodecFilter( 
                    new TextLineCodecFactory(Charset.forName("UTF-8"))))

        acceptor.setHandler(new RankServerHandler(mongoDB))
        acceptor.getSessionConfig().setReadBufferSize(2048)
        acceptor.getSessionConfig().setIdleTime(IdleStatus.BOTH_IDLE, 10)
        acceptor.bind(new InetSocketAddress(Config.serverPort))
    }
}
