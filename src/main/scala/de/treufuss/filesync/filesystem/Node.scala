package de.treufuss.filesync.filesystem

import org.apache.logging.log4j.scala.Logging

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Node[C](val id: Int,
              var name: String,
              var parent: Option[Node[C]],
              val children: mutable.TreeMap[String, Node[C]],
              var content: Option[C]) extends Logging {

  def find(nameSeq: Seq[String]): Option[Node[C]] = find(nameSeq.toList)

  def find(nameList: List[String]): Option[Node[C]] = nameList match {
      case head :: Nil => if (head == name) Some(this) else None
      case _ :: tail => children.get(tail.head).flatMap(_.find(tail))
    }

  private def nodesToRootRec(currentNode: Node[C], nodesSoFar: ListBuffer[Node[C]]): Unit = {
    nodesSoFar.prepend(currentNode)
    currentNode.parent match {
      case None =>
      case Some(p) => nodesToRootRec(p, nodesSoFar)
    }
  }

  def nodesToRoot: List[Node[C]] = {
    val list = ListBuffer.empty[Node[C]]
    nodesToRootRec(this, list)
    list.toList
  }

  def nodesPreOrder: Seq[Node[C]] = {
    if (children.isEmpty)
      Seq(this)
    else
      this +: children.values.toSeq.flatMap(_.nodesPreOrder)
  }

  def nodesPostOrder: Seq[Node[C]] = {
    if (children.isEmpty)
      Seq(this)
    else
      children.values.toSeq.flatMap(_.nodesPreOrder) :+ this
  }

  def toJson: String = {
    val childrenArray = if (children.nonEmpty) ",\"children\":[" + children.values.map(_.toJson).mkString(",") + "]"
    else ""

    val contentPara = content match {
      case None => ""
      case Some(c) => ",\"content\":\"" + c + "\""
    }

    s"""{"id":$id,"name":"$name"$contentPara$childrenArray}"""
  }

  override def toString: String = nodesToRoot.map(_.name).mkString("/")
}

object Node {
  def apply[C](id: Int, name: String, parent: Option[Node[C]], content: Option[C] = None) = new Node[C](
    id,
    name,
    parent,
    children = mutable.TreeMap.empty[String, Node[C]],
    content = content
  )
}