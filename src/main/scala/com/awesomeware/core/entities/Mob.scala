package com.awesomeware.core.entities

import com.awesomeware.core.Container
import com.awesomeware.core.DescType._

class Mob extends GameEntity with Container {
  def describeTo(to: GameEntity, dType: DescType): String = {
    dType match {
      case Name =>
        name
      case LongDesc =>
        this.description
    }
  }

  def description: String = {
    "Long descriptions not implemented yet."
  }

  def attemptMove(to: String, dir: Option[String]) {
    val lowerTo = to.toLowerCase
    location match {
      case r: Room =>
        r.getExit(lowerTo) match {
          case None => this.receiveText(s"No such exit as $to")
          case Some(exit) => this.move(exit.to, dir)
        }
      case _ =>
        throw new NotImplementedError("Haven't implemented movement in non-rooms yet.")
    }
  }
}