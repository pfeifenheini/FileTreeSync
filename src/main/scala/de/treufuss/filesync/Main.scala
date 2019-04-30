package de.treufuss.filesync

import java.time.Duration

import com.thedeanda.lorem.LoremIpsum
import de.treufuss.filesync.filesystem.{FileSystem, FileSystemConf}
import org.apache.logging.log4j.scala.Logging

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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

    val seed = Random.nextLong()
    //    val seed = 0L

    Random.setSeed(seed)

    val fs = FileSystem[String](fsConf)

    val lorem = new LoremIpsum(seed)

    val sizeLimit = 1000
    val operationLimit = 100000

    object RandomPath {

      private val fillSize = math.max(fs.size / 100, 10)

      private val q = mutable.Queue.empty[String]

      def next: String = {
        if (q.isEmpty) refill()
        q.dequeue()
      }

      def refill(): Unit = {
        val (idArray, size) = (fs.idSet.toArray, fs.size)
        1 to fillSize flatMap (_ => fs.pathOf(idArray(Random.nextInt(size)))) foreach (q.enqueue(_))
      }
    }

    println(s"fill file system with $sizeLimit nodes")
    var createIteration = 0
    while (fs.size < sizeLimit) {

      if (createIteration % (sizeLimit / 10) == 0) println(
        createIteration.toDouble / sizeLimit * 100 + "% - node count: " + fs.size)

      val path = RandomPath.next

      fs.create(path, lorem.getWords(1), Some(lorem.getWords(10, 20)))

      createIteration += 1
    }

    //    println(fs.toJson)

    println(s"benchmarking $operationLimit operations")

    timed {
      var successCounter = 0
      val successArray = ArrayBuffer.fill(5)(0)
      val totalArray = ArrayBuffer.fill(5)(0)
      1 to operationLimit foreach { iteration =>

        if (iteration % (operationLimit / 10) == 0) println(
          iteration.toDouble / operationLimit * 100 + "% - node count: " + fs.size)

        val path = RandomPath.next
        val dest = RandomPath.next

        Random.nextInt(5) match {
          case 0 =>
            totalArray(0) += 1
            if (fs.size < sizeLimit) {
              if (fs.create(path, lorem.getWords(1), None))
                successArray(0) += 1
            }
          case 1 =>
            totalArray(1) += 1
            if (fs.rename(path, lorem.getWords(1)))
              successArray(1) += 1
          case 2 =>
            totalArray(2) += 1
            if (fs.move(path, dest))
              successArray(2) += 1
          case 3 =>
            totalArray(3) += 1
            if (fs.size > sizeLimit / 2) {
              if (fs.delete(path))
                successArray(3) += 1
            }
          case 4 =>
            totalArray(4) += 1
            if (fs.edit(path, Some(lorem.getWords(10, 20))))
              successArray(4) += 1
          case _ =>
        }
      }

      val opNames = Array("create", "rename", "move", "delete", "edit")
      val stats = opNames.zip(totalArray.zip(successArray)).map {
        case (name, (total, success)) => (name, total, success, total - success)
      } map {
        case (name, total, success, fail) => "%10s | %10d | %10d | %10d".format(name, total, success, fail)
      }

      println("%10s | %10s | %10s | %10s".format("operation", "total", "success", "failure"))
      println("%10s | %10s | %10s | %10s".format("----------", "----------", "----------", "----------"))
      println(stats.mkString("\n"))

      //      println(s"total operations: $totalArray")
      //      println(s"successful operations: $successArray")
      //      println(s"failed operations: ${totalArray.zip(successArray).map(t => t._1 - t._2)}")
    }

    println("seed: " + seed)

    println("fs size " + fs.size)

    //    println(fs.toJson)

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
