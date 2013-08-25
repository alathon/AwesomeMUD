package awesomeware.commands.impl

import awesomeware.core.entities.GameEntity
import awesomeware.commands._
import awesomeware.core.{World, Container, DescType}
import awesomeware.core.entities.Room
import awesomeware.core.entities.Mob

object BasicUtilityCommands extends CommandGifter {
  val commands = Set[Command](new Who(), new Tell(), new Look(),
    new North(), new South(), new East(), new West(),
    new Say())

  def isEligable(cmd: Command, commander: Commander): Boolean = true
}

sealed class Look extends Command {
  val components = List(Or(Word("look"), Word("l")))
  val name = "look"

  def go(source: GameEntity, args: Seq[Any]) {
    val loc: Container = source.location
    val b = new StringBuilder()
    b ++= "You look around:\n\n"
    loc match {
      case room: Room =>
        b ++= room.describeTo(source, DescType.LongDesc)
      case _ =>
        b ++= "You see nothing at all! Oh no!"
    }
    source.receiveText(b.toString())
  }
}

sealed class Tell extends Command {
  val components = List(Word("tell"), Ref("mob", "here", optional = true), Anything(optional = true))
  val name = "tell"

  def go(source: GameEntity, args: Seq[Any]) {
    if (args.isEmpty) {
      source.receiveText("Tell who what?")
      return
    }

    args(0) match {
      case target: Mob =>
        if (args.length < 2) {
          source.receiveText(s"Tell ${target.name} what?")
        } else {
          source.receiveText(s"You tell ${target.name}, '${args(1)}'")
        }
      case _ =>
        source.receiveText("Tell who?")
    }
  }
}

sealed class Who extends Command {
  val components = List(Word("who"))
  val name = "who"

  def go(source: GameEntity, args: Seq[Any]) {
    val b = new StringBuilder()
    b ++= "---------------\r\n"
    World.clients.foreach(c => b ++= c.player.name + "\n")
    b ++= "---------------\r\n"
    b ++= "Players online: "
    b ++= World.clients.size.toString
    b ++= "\n"
    b ++= "---------------\r\n"
    source.receiveText(b.toString())
  }
}

sealed class North extends Command {
  val components = List(Or(Word("north"), Word("n")))
  val name = "north"

  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.attemptMove("north", Some("the north"))
      case _ =>
    }
  }
}

sealed class South extends Command {
  val components = List(Or(Word("south"), Word("s")))
  val name = "south"

  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.attemptMove("south", Some("the south"))
      case _ =>
    }
  }
}

sealed class East extends Command {
  val components = List(Or(Word("east"), Word("e")))
  val name = "east"

  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.attemptMove("east", Some("the east"))
      case _ =>
    }
  }
}

sealed class West extends Command {
  val components = List(Or(Word("west"), Word("w")))
  val name = "west"

  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.attemptMove("west", Some("the west"))
      case _ =>
    }
  }
}

sealed class Say extends Command {
  val components = List(Or(Word("say"), Word("'")), Anything(optional = true))
  val name = "say"

  def go(source: GameEntity, args: Seq[Any]) {
    if (args.length == 0) {
      source.receiveText("Say what??")
      return
    }

    for (entity <- (source.location.inventory diff List(source))) {
      entity.receiveText(s"${source.name} says, '${args(0)}#n'")
    }
    source.receiveText(s"You say, '${args(0)}#n'")
  }
}