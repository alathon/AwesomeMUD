package awesomeware.commands.impl

import awesomeware.commands.Command
import awesomeware.core.entities.GameEntity
import awesomeware.commands.Result
import awesomeware.commands._

object BasicUtilityCommands extends CommandGifter {
  val commands = Set[Command](WhoCommand)
  def isEligable(cmd: Command, commander: Commander): Boolean = true
}

object WhoCommand extends Command {
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