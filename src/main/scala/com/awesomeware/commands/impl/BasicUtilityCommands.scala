package com.awesomeware.commands.impl

import com.awesomeware.commands.{Word, Or, Commander, CommandGifter, Command}
import com.awesomeware.core.entities.{GameEntity, Room}
import com.awesomeware.core.{World, Container, DescType}
import com.awesomeware.core.entities.Mob
import com.awesomeware.core.DescType._

object BasicUtilityCommands extends CommandGifter {
  val commands = Set[Command](new Who(), new Tell(), new Look(), new Say(), new Quit())
}

sealed class Quit extends Command {
  val components = List(Word("quit"))
  val name = "quit"
  val category = "General"

  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.client match {
          case Some(c) =>
            source.receiveText("You quit.")

            for (entity <- source.location.inventory diff List(source)) {
              entity.receiveText(s"${source.describeTo(entity, Name)} quits.")
            }
            
            c.removeFromGame()
          case None => 
        }
      case _ => 
    }
  }
}

sealed class Look extends Command {
  val components = List(Or(Word("look"), Word("l")))
  val name = "look"
  val category = "General"
    
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
  val category = "General"
    
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


