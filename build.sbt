organization := "com.awkin"

name := "dylanrank"

initialize := false

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "com.mongodb.casbah" % "casbah_2.9.0-1" % "2.1.5.0" 

libraryDependencies += "org.apache.mina" % "mina-core" % "2.0.4"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.26"
