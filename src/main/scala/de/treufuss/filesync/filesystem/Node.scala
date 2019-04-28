package de.treufuss.filesync.filesystem

import org.apache.logging.log4j.scala.Logging

import scala.collection.mutable.ListBuffer

class Node[C](val id: Int,
              var name: String,
              var parent: Option[Node[C]],
              val children: ListBuffer[Node[C]],
              var content: Option[C]) extends Logging {

  def find(pathSeq: Seq[String]): Option[Node[C]] = pathSeq match {
    case head +: Seq() =>
      if (head == name) Some(this)
      else None
    case head +: tail => children.find(_.name == tail.head) match {
      case None => None
      case Some(c) => c.find(tail)
    }
  }

  def pathNodes: Seq[Node[C]] = parent match {
    case None => Seq(this)
    case Some(p) => p.pathNodes :+ this
  }

  def nodesPreOrder: Seq[Node[C]] = {
    if (children.isEmpty)
      Seq(this)
    else
      this +: children.sortWith((a, b) => a.name.compareTo(b.name) < 0).flatMap(_.nodesPreOrder)
  }

  def nodesPostOrder: Seq[Node[C]] = {
    if (children.isEmpty)
      Seq(this)
    else
      children.sortWith((a, b) => a.name.compareTo(b.name) < 0).flatMap(_.nodesPreOrder) :+ this
  }

  def toJson: String = {
    val childrenArray = if (children.nonEmpty) ",\"children\":[" + children.map(_.toJson).mkString(",") + "]"
    else ""

    val contentPara = content match {
      case None => ""
      case Some(c) => ",\"content\":\"" + c + "\""
    }

    s"""{"id":$id,"name":"$name"$contentPara$childrenArray}"""
  }

  override def toString: String = pathNodes.map(_.name).mkString("/")
}

object Node {
  def apply[C](id: Int, name: String, parent: Node[C], content: Option[C] = None) = new Node[C](
    id,
    name,
    parent = Some(parent),
    children = ListBuffer.empty[Node[C]],
    content = content
  )
}