package de.treufuss.filesync.filesystem

import org.apache.logging.log4j.scala.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class FileSystemConf(pathSeparator: Char,
                          rootName: String)

class FileSystem[C](config: FileSystemConf) extends Logging {

  private object IDGenerator {
    private var lastID = 0

    def nextID: Int = {
      lastID = lastID + 1
      lastID
    }
  }

  private val root: Node[C] = new Node[C](
    id = 0,
    name = config.rootName,
    parent = None,
    children = ListBuffer.empty[Node[C]],
    content = None
  )

  private val nodeIndex = mutable.HashMap(0 -> root)

  def size: Int = this.synchronized(nodeIndex.size)

  def idSet: collection.Set[Int] = this.synchronized(nodeIndex.keySet)

  def create(path: String, name: String, content: Option[C] = None): Boolean = this.synchronized {
    find(path) match {
      case None =>
        logger.error(s"cannot create, '$path' does not exist")
        false
      case Some(parent) =>
        if (parent.children.exists(_.name == name)) {
          logger.error(s"cannot create, name $name already exists at '$path'")
          false
        } else {
          val newNode = Node(IDGenerator.nextID, name, parent, content)
          parent.children += newNode
          nodeIndex.update(newNode.id, newNode)
          logger.info(s"create node '${pathOf(newNode)}'")
          true
        }
    }
  }

  private def deleteRecursive(toDelete: Node[C]): Unit = {
    assert(toDelete.parent.nonEmpty, "trying to delete root node")

    toDelete.children.foreach(deleteRecursive)
    val siblings = toDelete.parent.get.children
    siblings.remove(siblings.indexWhere(_.id == toDelete.id))
    nodeIndex.remove(toDelete.id)
  }

  def delete(path: String): Boolean = this.synchronized {
    find(path) match {
      case None =>
        logger.error(s"cannot delete, '$path' does not exist")
        false
      case Some(toDelete) =>
        toDelete.parent match {
          case None =>
            logger.error("cannot delete root node")
            false
          case Some(_) =>
            deleteRecursive(toDelete)
            logger.info(s"delete node '$path'")
            true
        }
    }
  }

  def move(source: String, dest: String): Boolean = this.synchronized {
    (find(source), find(dest)) match {
      case (None, _) =>
        logger.error(s"cannot move, source '$source' does not exist")
        false
      case (_, None) =>
        logger.error(s"cannot move, destination '$dest' does not exist")
        false
      case (Some(sourceNode), Some(destNode)) =>
        if (sourceNode.parent.isEmpty) {
          logger.error("cannot move root node")
          false
        }
        else if (destNode.pathNodes.exists(_.id == sourceNode.id)) {
          logger.error(s"cannot move node '${sourceNode.name}' into a sub node '$dest'")
          false
        }
        else
          destNode.children.find(_.name == sourceNode.name) match {
            case Some(_) =>
              logger.error(s"cannot move '$source' to '$dest', child with same name already exists")
              false
            case None =>
              val removeFrom = sourceNode.parent.get.children
              removeFrom.remove(removeFrom.indexWhere(_.id == sourceNode.id))

              destNode.children += sourceNode
              sourceNode.parent = Some(destNode)
              logger.info(s"move node '$source' to '$dest'")
              true
          }
    }
  }

  def edit(path: String, newContent: Option[C]): Boolean = this.synchronized {
    find(path) match {
      case Some(node) =>
        logger.info(s"edit node '$path'")
        node.content = newContent
        true
      case None =>
        logger.error(s"cannot edit, '$path' does not exist")
        false
    }
  }

  def rename(path: String, newName: String): Boolean = this.synchronized {

    if (newName == "") {
      logger.error("cannot rename, name cannot be empty string")
      return false
    }

    if (newName.contains(config.pathSeparator)) {
      logger.error(s"cannot rename, name must not contain path separator symbol ${config.pathSeparator}")
      return false
    }

    find(path) match {
      case Some(node) =>
        node.parent match {
          case None =>
            logger.error("cannot rename root node")
            false
          case Some(parent) if parent.children.exists(collision => collision.name == newName && collision.id != node.id) =>
            logger.error(s"cannot rename '$path' to $newName, name already taken")
            false
          case _ =>
            node.name = newName
            logger.info(s"rename node '$path' to $newName")
            true
        }
      case None =>
        logger.error(s"cannot rename, '$path' does not exist")
        false
    }
  }

  private def find(path: String): Option[Node[C]] = {
    val res = root.find(path.split(config.pathSeparator))
    if (res.isEmpty) logger.warn(s"node '$path' does not exist")
    res
  }

  private def find(id: Int): Option[Node[C]] = nodeIndex.get(id)

  private def pathOf(node: Node[C]): String = node.pathNodes.map(_.name).mkString(config.pathSeparator.toString)

  def pathOf(id: Int): Option[String] = this.synchronized {
    find(id) match {
      case Some(node) => Some(pathOf(node))
      case None => None
    }
  }

  def toJson: String = this.synchronized(root.toJson)

  override def toString: String = root.nodesPreOrder.map(node => pathOf(node) + " " + node.content.getOrElse("").toString).mkString("\n")
}