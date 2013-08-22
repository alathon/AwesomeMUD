package awesomeware.commands

import awesomeware.core.entities.GameEntity

abstract class CommandResult(val count: Integer, val command: Command, val output: Seq[Any]) extends Ordered[CommandResult] {
  def compare(that: CommandResult) = {
    this match {
      case c: ParseSuccess =>
        that match {
          case c2: ParseSuccess =>
            -that.count.compareTo(-this.count)
          case c2: ParseFailure =>
            -1
        }
      case c: ParseFailure =>
        that match {
          case c2: ParseSuccess =>
            1
          case c2: ParseFailure =>
            -that.count.compareTo(-this.count)
        }
    }
  }
}

case class ParseSuccess(override val count: Integer, override val command: Command, override val output: Seq[Any]) extends CommandResult(count, command, output)

case class ParseFailure(override val count: Integer, override val command: Command, override val output: Seq[Any]) extends CommandResult(count, command, output)

case class NoCommand() extends CommandResult(0, null, null)

object CommandInterpreter {
  def interpret[T >: CommandResult](text: String, source: GameEntity, commands: Set[Command]): T = {
    val tokens = text.split(" ")
    val input = ParseState(text, tokens, 0)
    val orderedCommands: List[CommandResult] = commands.map(x => x.parseInput(input, source)).toList.sorted
    val best = orderedCommands.head

    best.count.toInt match {
      case 0 =>
        NoCommand()
      case _ =>
        best match {
          case c: ParseFailure if c.count == 0 =>
            NoCommand()
          case _ =>
            best
        }
    }
  }
}