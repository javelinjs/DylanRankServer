package com.awkin.dylanrank

class Config private {
    private var serverPort: Int = _
    private var db: String = _
}

object Config {
    private val conf = new Config()
    def readConf(): Boolean = {
        conf serverPort_= 8193
        conf db_= "awkin"
        true
    }

    def serverPort = conf.serverPort
    def db = conf.db
}
