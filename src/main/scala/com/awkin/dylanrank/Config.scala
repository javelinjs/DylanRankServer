package com.awkin.dylanrank

import org.json._
import java.io._

import scala.io.Source

class Config private {
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
    private val confFile = "config/dylan.conf"

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

    def readConf() {
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
                            println(ex.getMessage())
                    }
                case isComment() => 
                }
            }
        } catch {
            case exFile: FileNotFoundException =>
                val ex = new InvalidConfFile(confFile)
                println(ex.getMessage)
                println("[Config] Use default config")
            case exUnknown =>
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
