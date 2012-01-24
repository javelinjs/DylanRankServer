package com.awkin.dylanrank

import org.json._
import java.io._

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import scala.io.Source

class Config private {
    val logger = LoggerFactory.getLogger(classOf[Config])

    var pair: JSONObject = new JSONObject()

    private var serverPortDef: Int = 8193
    private var dbDef: String = "awkin"
    private var dbHostDef: String = "localhost"
    private var dbPortDef: Int = 27017
    private var dbUserDef: String = ""
    private var dbPwdDef: String = ""
}

object Config {
    private val conf = new Config()
    private val confFileDef = "config/dylan.conf"

    def db = 
        conf.pair.optString("db", conf.dbDef)
    def dbHost = 
        conf.pair.optString("db_host", conf.dbHostDef)
    def dbPort = 
        conf.pair.optInt("db_port", conf.dbPortDef)
    def dbUser = 
        conf.pair.optString("db_user", conf.dbUserDef)
    def dbPwd = 
        conf.pair.optString("db_pwd", conf.dbPwdDef)
    def serverPort = 
        conf.pair.optInt("serverPort", conf.serverPortDef)

    def readConf(filename: Option[String] = None) {
        val confFile = filename.getOrElse(confFileDef)
        conf.logger.info("read config from {}", confFile)
        try {
            Source.fromFile(confFile).getLines.foreach { line =>
                line match {
                case notComment() =>
                    try {
                        val setting = new JSONObject("{" + line + "}")
                        val key = setting.keys.next.toString()
                        conf.pair.put(key, setting.get(key))
                    } catch {
                        case _ =>
                            val ex = new InvalidConfSetting(confFile, line)
                            conf.logger.warn(ex.getMessage())
                    }
                case isComment() => 
                }
            }
        } catch {
            case exFile: FileNotFoundException =>
                val ex = new InvalidConfFile(confFile)
                conf.logger.warn(ex.getMessage)
                conf.logger.info("use default config")
            case exUnknown =>
                conf.logger.error("unable to read config from {}", confFile) 
                throw exUnknown
        }
    }
}

object isComment {
    def apply(symbol: String) : Boolean = symbol(0) == '#'
    def unapply(symbol: String) : Boolean = symbol(0) == '#'
}
object notComment {
    def apply(symbol: String) : Boolean = !isComment.apply(symbol)
    def unapply(symbol: String) : Boolean = !isComment.unapply(symbol)
}

/* Exceptions */
class InvalidConfFile(val confFile: String) extends Exception {
    override def getMessage() = {
        "No such conf file: %s".format(confFile)
    }
}
class InvalidConfSetting(val confFile: String, val setting: String) extends Exception {
    override def getMessage() = {
        "Invalid setting of %s in file %s".format(setting, confFile)
    }
}
