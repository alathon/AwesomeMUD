package com.awesomeware.commands.impl

import scala.Some
import com.awesomeware.commands.{Commander, Word, Or, CommandGifter, Command}
import com.awesomeware.core.entities.{Mob, GameEntity}

object MovementCommands extends CommandGifter {
  val commands = Set[Command](new North(), new South(), new East(), new West())
}


sealed class North extends Command {
  val components = List(Or(Word("north"), Word("n")))
  val name = "north"
  val category = "Movement"
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
  val category = "Movement"
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
  val category = "Movement"
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
  val category = "Movement"
  def go(source: GameEntity, args: Seq[Any]) {
    source match {
      case m: Mob =>
        m.attemptMove("west", Some("the west"))
      case _ =>
    }
  }
}