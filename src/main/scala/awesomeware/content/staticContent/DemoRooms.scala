package awesomeware.content.staticContent

import awesomeware.core.entities.{ContainerExit, Room}

object OtherVoid extends Room {
  description = "This void is ALSO full of emtpiness."
  name = "The Other Void"
  exits = Set(new ContainerExit(Set("south"), this, TheVoid), new ContainerExit(Set("north"), this, TheVoid))
}

object TheVoid extends Room {
  description = "The void is full of emptiness."
  name = "The Void"
  exits = Set(new ContainerExit(Set("north"), this, OtherVoid))
}