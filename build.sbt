name := "FileTreeSync"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(

  // logging framework
  "org.apache.logging.log4j" % "log4j-api" % "2.11.2",
  "org.apache.logging.log4j" % "log4j-core" % "2.11.2",
  "org.apache.logging.log4j" %% "log4j-api-scala" % "11.0",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test,

  // lorem ipsum generator
  "com.thedeanda" % "lorem" % "2.1"

)