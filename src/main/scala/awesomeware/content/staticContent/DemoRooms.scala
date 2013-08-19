package awesomeware.content.staticContent

import awesomeware.core.entities.Room
import awesomeware.core.entities.RoomExit

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