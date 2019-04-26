package de.treufuss.filesync

import java.time.Duration

import com.thedeanda.lorem.LoremIpsum
import de.treufuss.filesync.filesystem.{FileSystem, FileSystemConf}
import org.apache.logging.log4j.scala.Logging

import scala.util.Random

object Main extends App with Logging {

  logger.info("Program started")

  //  val seed = Random.nextLong()
  val seed = 0L

  Random.setSeed(seed)

  val fsConf = FileSystemConf(
    pathSeparator = '/',
    rootName = "root"
  )

  val fs = new FileSystem[String](fsConf)

  val lorem = new LoremIpsum(seed)

  def randomPathWithError = {
    val idSet = fs.idSet
    fs.pathOf(idSet.iterator.drop(Random.nextInt(idSet.size)).next).get + (if (Random.nextDouble() < 0.001) "a" else "")
  }

  def timed[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()

    val dur = Duration.ofNanos(t1 - t0)
    println("Elapsed time: " + dur.toString.stripPrefix("PT"))
    result
  }

  val sizeLimit = 1000
  val operationLimit = 1000000

  while (fs.size < sizeLimit) {
    val path = randomPathWithError

    fs.create(path, lorem.getWords(1))
  }

  timed {
    1 to operationLimit foreach { iteration =>

      if (iteration % (operationLimit / 10) == 0) println(iteration.toDouble / operationLimit * 100 + "%")

      val path = randomPathWithError
      val dest = randomPathWithError

      Random.nextInt(4) match {
        case 0 =>
          if (fs.size < sizeLimit) {
            fs.create(path, lorem.getWords(1))
          }
        case 1 =>
          fs.rename(path, lorem.getWords(1))
        case 2 =>
          if (!fs.move(path, dest)) fs.move(dest, path)
        case 3 =>
          if (fs.size > sizeLimit / 2) {
            fs.delete(path)
          }
      }
    }
  }

  println("seed: " + seed)

  //  println(fs)
  //  println(fs.toJson)
}
