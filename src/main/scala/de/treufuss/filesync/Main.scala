package de.treufuss.filesync

import java.time.Duration

import com.thedeanda.lorem.LoremIpsum
import de.treufuss.filesync.filesystem.{FileSystem, FileSystemConf}
import org.apache.logging.log4j.scala.Logging

import scala.util.Random

object Main extends App with Logging {

  logger.info("Program started")

  val fsConf = FileSystemConf(
    pathSeparator = '/',
    rootName = "root"
  )

//  miniTest

  benchmark

  def miniTest = {

    val fs = FileSystem[String](fsConf)
    println(fs)
    fs.create("root", "a", "First Node")
    println(fs)
    fs.create("root", "b", "Second Node")
    println(fs)
    fs.create("root/a", "c", "Third Node")
    println(fs)
    fs.move("root/a/c", "root/b")
    println(fs)
    fs.rename("root/b/c", "d")
    println(fs)
    fs.edit("root/b/d", "hallo welt")
    println(fs)
    fs.delete("root/b")
    println(fs)

  }

  def benchmark = {

    logger.info("start benchmark")

    //  val seed = Random.nextLong()
    val seed = 0L

    Random.setSeed(seed)

    val fs = FileSystem[String](fsConf)

    val lorem = new LoremIpsum(seed)

    val sizeLimit = 100000
    val operationLimit = 1000000

    var createIteration = 0

    println(s"fill file system with $sizeLimit nodes")
    while (fs.size < sizeLimit) {

      if (createIteration % (operationLimit / 10) == 0) println(createIteration.toDouble / sizeLimit * 100 + "%")

      val path = randomFastPath

      fs.create(path, lorem.getWords(1), Some(lorem.getWords(10, 20)))

      createIteration += 1
    }

    println(s"benchmarking $operationLimit operations")

    timed {
      var successCounter = 0
      1 to operationLimit foreach { iteration =>

        if (iteration % (operationLimit / 10) == 0) println(iteration.toDouble / operationLimit * 100 + "%")

        val path = randomFastPathWithError
        val dest = randomFastPathWithError

        Random.nextInt(5) match {
//          case 0 => if (fs.size < sizeLimit) fs.create(path, lorem.getWords(1), None)
          case 1 => if (fs.rename(path, lorem.getWords(1))) successCounter += 1
          case 2 => if (fs.move(path, dest)) successCounter += 1
//          case 3 => if (fs.size > sizeLimit / 2) fs.delete(path)
          case 4 => if (fs.edit(path, Some(lorem.getWords(10, 20)))) successCounter += 1
          case _ =>
        }
      }
      println(s"successful operations: $successCounter of $operationLimit")
    }

    println("seed: " + seed)

    println("fs size " + fs.size)

//    println(fs.toJson)

    def randomFastPathWithError = fs.synchronized {
      val RandomId = Random.nextInt(fs.size)
      fs.pathOf(RandomId).getOrElse("") + (if (Random.nextDouble() < 0.001) "a" else "")
    }

    def randomPathWithError = fs.synchronized {
      val idSet = fs.idSet
      val RandomId = idSet.toArray.apply(Random.nextInt(fs.size))
      fs.pathOf(RandomId).getOrElse("") + (if (Random.nextDouble() < 0.001) "a" else "")
    }

    def randomFastPath = fs.synchronized {
      val RandomId = Random.nextInt(fs.size)
      fs.pathOf(RandomId).getOrElse("")
    }

    def randomPath = fs.synchronized {
      val idSet = fs.idSet
      val RandomId = idSet.toArray.apply(Random.nextInt(fs.size))
      fs.pathOf(RandomId).getOrElse("")
    }

    def timed[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()

      val dur = Duration.ofNanos(t1 - t0)
      println("Elapsed time: " + dur.toString.stripPrefix("PT"))
      result
    }
  }




}
