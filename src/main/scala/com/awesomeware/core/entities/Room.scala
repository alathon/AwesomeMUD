package com.awesomeware.core.entities

import scala.Predef._
import scala.Some
import com.awesomeware.core.Container
import com.awesomeware.core.DescType._

case class RoomExit(from: Room, to: Container)

class ContainerExit(val names: Set[String], val from: Container, val to: Container) {
  override def toString: String = names.head(0).toUpper + names.head.substring(1)
}

class Room extends GameEntity with Container {
  var description: String = "An empty room."

  protected var exits: Set[ContainerExit] = Set()

  def getAllExits: Set[String] = {
    exits.flatMap(x => x.names)
  }

  def getExit(name: String): Option[ContainerExit] = {
    val lowerName = name.toLowerCase
    val valid: Set[ContainerExit] = exits.filter({
      exit => exit.names.contains(lowerName)
    })
    valid.size match {
      case 0 => None
      case _ => Some(valid.head)
    }
  }

  def describeTo(to: GameEntity, dType: DescType): String = {
    var b = new StringBuilder()

    b ++= s"$name\n"
    b ++= "Exits: "

    exits.size match {
      case 0 =>
        b ++= "None\n"
      case _ =>
        b ++= exits.mkString(", ")
        b ++= "\n"
    }
    b ++= "-" * name.length() + "\n"


    b ++= description + "\n"
    b ++= "-" * name.length() + "\n"

    b.toString()
  }

  override def entered(obj: GameEntity, from: Container) {
    for (e <- inventory diff List(obj)) {
      var text = s"${obj.describeTo(e, Name)} arrives"
      obj.direction match {
        case None => text += "."
        case Some(dir) => text += " from ${dir}."
      }
      e.receiveText(text)
    }
    obj.receiveText(this.describeTo(obj, LongDesc))
  }

  override def exited(obj: GameEntity, to: Container) {
    for (e <- inventory) {
      var text = s"${obj.describeTo(e, Name)} leaves"
      obj.direction match {
        case None => text += "."
        case Some(dir) => text += " towards ${dir}."
      }
      e.receiveText(text)
    }
  }
}