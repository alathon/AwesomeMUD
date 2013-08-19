package awesomeware.core

object OtherVoid extends Room {
  description = "This void is ALSO full of emtpiness."
  name = "The Other Void"
  exits = Set(new RoomExit("south", this, TheVoid))
}

object TheVoid extends Room {
  description = "The void is full of emptiness."
  name = "The Void"
  exits = Set(new RoomExit("north", this, OtherVoid))
}

class RoomExit(cmd:String, fromRoom:Room, toRoom:Room) extends GameEntity{
  var command:String = _
  var from:Room = fromRoom
  var to:Room = toRoom
}

class Room extends GameEntity with Container{
  var description:String = "An empty room."
  var name:String = "Unknown Room"

  protected var exits: Set[RoomExit] = Set()
}