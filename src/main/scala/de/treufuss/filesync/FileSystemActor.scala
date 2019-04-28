package de.treufuss.filesync

import akka.actor.Actor
import de.treufuss.filesync.filesystem.FileSystem
import de.treufuss.filesync.filesystem.operations._

class FileSystemActor[C](private val fs: FileSystem[C]) extends Actor {

  override def receive: Receive = {
    case Create(path, name, content: Option[C]) => fs.create(path, name, content)
    case Move(path, dest) => fs.move(path, dest)
    case Rename(path, newName) => fs.rename(path, newName)
    case Edit(path, newContent: Option[C]) => fs.edit(path, newContent)
    case Delete(path) => fs.delete(path)
    case _ =>
  }

}
