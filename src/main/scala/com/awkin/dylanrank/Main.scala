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

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection

object Main {
    def main(args: Array[String]) = {
        Config.readConf()

        val mongoConn = MongoConnection()
        /*
        val docService = new Document()
        docService.start() */

        val acceptor: IoAcceptor = new NioSocketAcceptor

        acceptor.getFilterChain().addLast("logger", new LoggingFilter())
        acceptor.getFilterChain().addLast("codec", 
                new ProtocolCodecFilter( 
                    new TextLineCodecFactory(Charset.forName("UTF-8"))))

        acceptor.setHandler(new CtrServerHandler(mongoConn))
        acceptor.getSessionConfig().setReadBufferSize(2048)
        acceptor.getSessionConfig().setIdleTime(IdleStatus.BOTH_IDLE, 10)
        acceptor.bind(new InetSocketAddress(Config.serverPort))
    }
}
