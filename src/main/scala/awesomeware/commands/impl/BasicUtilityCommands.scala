package awesomeware.commands.impl

import awesomeware.core.entities.GameEntity
import awesomeware.commands._
import awesomeware.core.Container
import awesomeware.core.entities.Room
import awesomeware.core.DescType
import awesomeware.core.entities.Mob

object BasicUtilityCommands extends CommandGifter {
  val commands = Set[Command](new Who(), new Tell(), new Look())

  def isEligable(cmd: Command, commander: Commander): Boolean = true
}

sealed class Look extends Command {
  val components = List(Word("look"))
  val name = "look"

  def go(source: GameEntity, args: Seq[Any]) {
    val loc: Container = source.location
    val b = new StringBuilder()
    b ++= "You look around:\n"
    loc match {
      case room: Room =>
        b ++= room.describeTo(source, DescType.LongDesc)
      case _ =>
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
    b ++= "(Who goes here)\r\n"
    b ++= "---------------\r\n"
    source.receiveText(b.toString())
  }
}