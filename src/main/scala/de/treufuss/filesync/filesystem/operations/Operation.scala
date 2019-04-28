package de.treufuss.filesync.filesystem.operations

sealed trait Operation {
  def path: String
}

case class Create[C](path: String, name: String, content: Option[C] = None) extends Operation

case class Move(path: String, dest: String) extends Operation

case class Edit[C](path: String, newContent: Option[C]) extends Operation

case class Delete(path: String) extends Operation

case class Rename(path: String, newName: String) extends Operation