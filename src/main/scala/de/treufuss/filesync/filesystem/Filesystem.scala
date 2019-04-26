package de.treufuss.filesync.filesystem

import org.apache.logging.log4j.scala.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class FileSystem[C](config: FileSystemConf) extends Logging {

  private object IDGenerator {
    private var lastID = 0

    def nextID: Int = {
      lastID = lastID + 1
      lastID
    }
  }

  val root: Node[C] = new Node[C](
    id = 0,
    name = config.rootName,
    parent = None,
    children = ListBuffer.empty[Node[C]],
    content = None
  )

  private val nodeIndex = mutable.HashMap(0 -> root)

  def idSet: collection.Set[Int] = nodeIndex.keySet

  def create(path: String, name: String): Option[Node[C]] = find(path) match {
    case None => None
    case Some(parent) =>
      if (parent.children.exists(_.name == name)) {
        logger.error(s"cannot create, name $name already exists at '$path'")
        None
      } else {
        val newNode = Node(IDGenerator.nextID, name, parent)
        parent.children += newNode
        nodeIndex.update(newNode.id, newNode)
        logger.info(s"create node '${pathOf(newNode)}'")
        Some(newNode)
      }
  }

  def delete(path: String): Option[Node[C]] = {
    find(path) match {
      case None => None
      case Some(toDelete) =>
        toDelete.parent match {
          case None =>
            logger.error("cannot delete root node")
            None
          case Some(parent) =>
            parent.children.remove(parent.children.indexWhere(_.id == toDelete.id))
            nodeIndex.remove(toDelete.id)
            logger.info(s"delete node '$path'")
            Some(parent)
        }
    }
  }

  def move(source: String, dest: String): Option[Node[C]] = {
    (find(source), find(dest)) match {
      case (None, _) =>
        logger.error(s"cannot move, source '$source' does not exist")
        None
      case (_, None) =>
        logger.error(s"cannot move, destination '$dest' does not exist")
        None
      case (Some(sourceNode), Some(destNode)) =>
        if (sourceNode.parent.isEmpty) {
          logger.error("cannot move root node")
          None
        }
        else if (destNode.pathNodes.exists(_.id == sourceNode.id)) {
          logger.error(s"cannot move node '${sourceNode.name}' into a sub node '$dest'")
          None
        }
        else
          destNode.children.find(_.name == sourceNode.name) match {
            case Some(_) =>
              logger.error(s"cannot move '$source' to '$dest', child with same name already exists")
              None
            case None =>
              val removeFrom = sourceNode.parent.get.children
              removeFrom.remove(removeFrom.indexWhere(_.id == sourceNode.id))

              destNode.children += sourceNode
              sourceNode.parent = Some(destNode)
              logger.info(s"move node '$source' to '$dest'")
              Some(sourceNode)
          }
    }
  }

  def edit(path: String, newContent: C): Unit = find(path) match {
    case Some(node) =>
      logger.info(s"edit node '$path'")
      node.content = Some(newContent)
    case None =>
  }

  def rename(path: String, newName: String): Option[String] = {

    if (newName == "") {
      logger.error("cannot rename, name cannot be empty string")
      return None
    }

    if (newName.contains(config.pathSeparator)) {
      logger.error(s"cannot rename, name must not contain path separator symbol ${config.pathSeparator}")
      return None
    }

    find(path) match {
      case Some(node) =>
        node.parent match {
          case None =>
            logger.error("cannot rename root node")
            None
          case Some(parent) if parent.children.exists(collision => collision.name == newName && collision.id != node.id) =>
            logger.error(s"cannot rename '$path' to $newName, name already taken")
            None
          case _ =>
            node.name = newName
            logger.info(s"rename node '$path' to $newName")
            Some(newName)
        }
      case None => None
    }
  }

  def find(path: String): Option[Node[C]] = {
    val res = root.find(path.split(config.pathSeparator))
    if (res.isEmpty) logger.error(s"node '$path' does not exist")
    res
  }

  def find(id: Int): Option[Node[C]] = nodeIndex.get(id)

  def pathOf(node: Node[C]): String = node.pathNodes.map(_.name).mkString(config.pathSeparator.toString)

  def pathOf(id: Int): Option[String] = find(id) match {
    case Some(node) => Some(pathOf(node))
    case None => None
  }

  def toJson: String = root.toJson

  override def toString: String = root.nodesPreOrder.map(node => pathOf(node) + " " + node.content.getOrElse("").toString).mkString("\n")
}

case class FileSystemConf(pathSeparator: Char,
                          rootName: String)