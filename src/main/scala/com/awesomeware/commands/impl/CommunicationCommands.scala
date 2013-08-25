package com.awesomeware.commands.impl

import com.awesomeware.commands._
import com.awesomeware.core.entities.{Mob, GameEntity}


object CommunicationCommands extends CommandGifter {
  val commands = Set[Command](new Tell(), new Say())

  def isEligable(cmd: Command, commander: Commander): Boolean = true
}


sealed class Say extends Command {
  val components = List(Or(Word("say"), Word("'")), Anything(optional = true))
  val name = "say"

  def go(source: GameEntity, args: Seq[Any]) {
    if (args.length == 0) {
      source.receiveText("Say what??")
      return
    }

    for (entity <- source.location.inventory diff List(source)) {
      entity.receiveText(s"${source.name} says, '${args(0)}#n'")
    }
    source.receiveText(s"You say, '${args(0)}#n'")
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