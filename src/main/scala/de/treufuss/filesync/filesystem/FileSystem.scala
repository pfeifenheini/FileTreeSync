package de.treufuss.filesync.filesystem

import org.apache.logging.log4j.scala.Logging

import scala.collection.{Set, mutable}

case class FileSystemConf(pathSeparator: Char,
                          rootName: String)

trait FileSystem[C] {

  def size: Int

  def idSet: Set[Int]

  def create(path: String, name: String, content: Option[C]): Boolean

  def delete(path: String): Boolean

  def move(path: String, dest: String): Boolean

  def edit(path: String, newContent: Option[C]): Boolean

  def rename(path: String, newName: String): Boolean

  def pathOf(id: Int): Option[String]

  def toJson: String
}

object FileSystem {
  def apply[C](conf: FileSystemConf): FileSystem[C] = new FileSystemImpl[C](conf)
}

class FileSystemImpl[C](config: FileSystemConf) extends FileSystem[C] with Logging {

  private object IDGenerator {
    private var lastID = 0

    def nextID: Int = {
      lastID = lastID + 1
      lastID
    }
  }

  private val root: Node[C] = Node.apply(
    id = 0,
    name = config.rootName,
    parent = None,
    content = None
  )

  private val nodeIndex = mutable.TreeMap(0 -> root)

  override def size: Int = this.synchronized(nodeIndex.size)

  override def idSet: Set[Int] = this.synchronized(nodeIndex.keySet)

  override def create(path: String, name: String, content: Option[C] = None): Boolean = this.synchronized {
    find(path) match {
      case None =>
        logger.error(s"cannot create, '$path' does not exist")
        false
      case Some(parent) =>
        if (parent.children.contains(name)) {
          logger.error(s"cannot create, name $name already exists at '$path'")
          false
        } else {
          val newNode = Node(IDGenerator.nextID, name, Some(parent), content)
          parent.children.update(name, newNode)
          nodeIndex.update(newNode.id, newNode)
          logger.info(s"create node '${pathOf(newNode)}'")
          true
        }
    }
  }

  override def delete(path: String): Boolean = this.synchronized {
    find(path) match {
      case None =>
        logger.error(s"cannot delete, '$path' does not exist")
        false
      case Some(toDelete) =>
        toDelete.parent match {
          case None =>
            logger.error("cannot delete root node")
            false
          case Some(parent) =>
            toDelete.nodesPreOrder.map(_.id) foreach nodeIndex.remove
            parent.children.remove(toDelete.name)
            logger.info(s"delete node '$path'")
            true
        }
    }
  }

  override def move(path: String, dest: String): Boolean = this.synchronized {
    (find(path), find(dest)) match {
      case (None, _) =>
        logger.error(s"cannot move, source '$path' does not exist")
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
          destNode.children.get(sourceNode.name) match {
            case Some(_) =>
              logger.error(s"cannot move '$path' to '$dest', child with same name already exists")
              false
            case None =>
              sourceNode.parent.get.children.remove(sourceNode.name)
              destNode.children.update(sourceNode.name, sourceNode)
              sourceNode.parent = Some(destNode)
              logger.info(s"move node '$path' to '$dest'")
              true
          }
    }
  }

  override def edit(path: String, newContent: Option[C]): Boolean = this.synchronized {
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

  override def rename(path: String, newName: String): Boolean = this.synchronized {

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
          case Some(parent) if parent.children.contains(newName) =>
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

  override def pathOf(id: Int): Option[String] = this.synchronized {
    find(id) match {
      case Some(node) => Some(pathOf(node))
      case None => None
    }
  }

  override def toJson: String = this.synchronized(root.toJson)

  override def toString: String = root.nodesPreOrder.map(node => pathOf(node) + " " + node.content.getOrElse("").toString).mkString("\n")
}