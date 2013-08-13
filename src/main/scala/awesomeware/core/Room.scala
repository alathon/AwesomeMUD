package awesomeware.core

object TheVoid extends Room {
  description = "The void is full of emptiness."
  name = "The Void"
}

class Room extends GameEntity with Container{
  var description:String = ""
  var name:String = ""
}
