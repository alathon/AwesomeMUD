package awesomeware.core.entities

import awesomeware.core.DescType._
import awesomeware.core.Container
import awesomeware.core.DescType

class RoomExit(cmd: String, fromRoom: Room, toRoom: Room) extends GameEntity {
  var command: String = _
  var from: Room = fromRoom
  var to: Room = toRoom

  def describeTo(to: GameEntity, dType: DescType): String = {
    return "A room exit."
  }
}

class Room extends GameEntity with Container {
  var description: String = "An empty room."

  protected var exits: Set[RoomExit] = Set()

  def describeTo(to: GameEntity, dType: DescType): String = {
    var b = new StringBuilder()

    b ++= s"$name\n"
    b ++= "-" * name.length() + "\n"
    b ++= description + "\n"
    b ++= "-" * name.length() + "\n"

    return b.toString
  }

  override def entered(obj: GameEntity, from: Container) {
    for (e <- (inventory diff List(obj))) {
      e.receiveText(s"$obj entered from $from")
    }
    obj.receiveText(this.describeTo(obj, DescType.LongDesc))
  }

  override def exited(obj: GameEntity, to: Container) {
    for (e <- inventory) {
      e.receiveText(s"$obj left towards $to")
    }
  }
}