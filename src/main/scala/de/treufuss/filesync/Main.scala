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

  val fs = FileSystem[String](fsConf)

  val lorem = new LoremIpsum(seed)

  def randomPathWithError = fs.synchronized {
    val idSet = fs.idSet
    val index = idSet.iterator.drop(Random.nextInt(idSet.size)).next
    fs.pathOf(index).getOrElse("") + (if (Random.nextDouble() < 0.001) "a" else "")
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

//    fs.create(path, lorem.getWords(1))

    fs.create(path, lorem.getWords(1), Some(lorem.getWords(10, 20)))
  }

  timed {
    1 to operationLimit foreach { iteration =>

      if (iteration % (operationLimit / 10) == 0) println(iteration.toDouble / operationLimit * 100 + "%")

      val path = randomPathWithError
      val dest = randomPathWithError

      Random.nextInt(5) match {
        case 0 => if (fs.size < sizeLimit) fs.create(path, lorem.getWords(1), None)
        case 1 => fs.rename(path, lorem.getWords(1))
        case 2 => fs.move(path, dest)
        case 3 => if (fs.size > sizeLimit / 2) fs.delete(path)
        case 4 => fs.edit(path, Some(lorem.getWords(10, 20)))
      }
    }
  }

  println("seed: " + seed)

  logger.info("giving actor time to finish")
  Thread.sleep(2000)
  logger.info("thats been enough time")

  println(fs.toJson)
}
