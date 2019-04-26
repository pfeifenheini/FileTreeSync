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

  def randomPathWithError = {
    val idSet = fs.idSet
    fs.pathOf(idSet.iterator.drop(Random.nextInt(idSet.size)).next).get + (if (Random.nextDouble() < 0.001) "a" else "")
  }

  while (fs.size < 1000) {
    val path = randomPathWithError

    fs.create(path, lorem.getWords(1))
  }

  1 to 100 foreach { _ =>

    Random.nextInt(4) match {
      case 0 =>
        while (fs.size < 1000) {
          val path = randomPathWithError

          fs.create(path, lorem.getWords(1))
        }
      case 1 =>
        1 to 5000 foreach { _ =>
          val path = randomPathWithError

          fs.rename(path, lorem.getWords(1))
        }
      case 2 =>
        1 to 5000 foreach { _ =>
          val source = randomPathWithError
          val dest = randomPathWithError

          fs.move(source, dest)
        }
      case 3 =>
        while (fs.size > 500) {
          val path = randomPathWithError

          fs.delete(path)
        }
    }
  }

  println(fs)
  println(fs.toJson)
}
