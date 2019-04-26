package de.treufuss.filesync

import de.treufuss.filesync.filesystem.{FileSystem, FileSystemConf}
import org.apache.logging.log4j.scala.Logging
import com.thedeanda.lorem.LoremIpsum

import scala.util.Random

object Main extends App with Logging {

  logger.info("Program started")

  val fsConf = FileSystemConf(
    pathSeparator = '/',
    rootName = "root"
  )

  val fs = new FileSystem[String](fsConf)

  val lorem = LoremIpsum.getInstance

  def randomPath = {
    val idSet = fs.idSet
    fs.pathOf(idSet.iterator.drop(Random.nextInt(idSet.size)).next).get
  }

  1 to 100 foreach  { _ =>
    val path = randomPath

    fs.create(path,lorem.getWords(1))
  }

  1 to 100 foreach { _ =>
    val path = randomPath

    fs.rename(path,lorem.getWords(1))
  }

  1 to 500 foreach { _ =>
    val source = randomPath
    val dest = randomPath

    fs.move(source, dest)
  }

//  while (fs.idSet.size > 50) {
//    val path = randomPath
//
//    fs.delete(path)
//  }

  println(fs)
  println(fs.toJson)
}
