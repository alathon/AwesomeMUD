package awesomeware.commands.impl

import awesomeware.commands.Command
import awesomeware.core.entities.GameEntity
import awesomeware.commands.Result
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
  val components = List(new Word("look"))
  val name = "look"

  def go(source: GameEntity, args: Seq[Result[_]]) {
    val loc:Container = source.location
    val b = new StringBuilder()
    b ++= "You look around:\n"
    if(loc.isInstanceOf[Room]) {
      val room:Room = loc.asInstanceOf[Room]
      b ++= room.describeTo(source, DescType.LongDesc)
    }
    source.receiveText(b.toString())
  }
}

  sealed class Tell extends Command {
    val components = List(new Word("tell"), new Ref("mob","here",optional=true), new Anything(optional=true))
    val name = "tell"

    def go(source: GameEntity, args: Seq[Result[_]]) {
      if(args.isEmpty) {
        source.receiveText("Tell who what?")
        return
      }

      val res0 = args(0).getResult
      if(res0.isInstanceOf[Mob]) {
        val target:Mob = res0.asInstanceOf[Mob]
        if(args.length < 2) {
          source.receiveText(s"Tell ${target.name} what?")
          return
        }
        val data = args(1).getResult
        source.receiveText(s"You tell ${target.name}, '$data'")
      } else {
        source.receiveText("Tell who?")
      }
    }
  }

  sealed class Who extends Command {
    val components = List(new Word("who"))
    val name = "who"

    def go(source: GameEntity, args: Seq[Result[_]]) {
      val b = new StringBuilder()
      b ++= "---------------\n"
      b ++= "(Who goes here)\n"
      b ++= "---------------\n"
      source.receiveText(b.toString)
    }
  }