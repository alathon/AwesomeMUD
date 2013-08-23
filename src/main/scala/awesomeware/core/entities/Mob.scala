package awesomeware.core.entities

import awesomeware.core.DescType._
import awesomeware.core.Container

class Mob extends GameEntity with Container {
  def describeTo(to: GameEntity, dType: DescType): String = {
    dType match {
      case ShortDesc =>
        return shortDesc(to)
      case LongDesc =>
        return longDesc(to)
    }
  }

  def shortDesc(to: GameEntity): String = this.name

  def longDesc(to: GameEntity): String = this.name

  def attemptMove(to: String) {
    val lowerTo = to.toLowerCase
    location match {
      case r: Room =>
        r.getAllExits().contains(lowerTo) match {
          case true =>
            r.getExit(lowerTo) match {
              case None =>
              case Some(exit) =>
                this.move(exit.to)
            }
          case false =>
            this.receiveText(s"No such exit as $to")
        }
      case _ =>
        throw new NotImplementedError("Haven't implemented movement in non-rooms yet.")
    }
  }
}