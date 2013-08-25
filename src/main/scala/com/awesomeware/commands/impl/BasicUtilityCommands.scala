package com.awesomeware.commands.impl

import com.awesomeware.commands.{Word, Or, Commander, CommandGifter, Command}
import com.awesomeware.core.entities.{GameEntity, Room}
import com.awesomeware.core.{World, Container, DescType}


object BasicUtilityCommands extends CommandGifter {
  val commands = Set[Command](new Who(), new Tell(), new Look(),
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


