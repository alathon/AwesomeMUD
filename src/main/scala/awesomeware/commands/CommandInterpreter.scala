package awesomeware.commands

import scala.collection.mutable.ArrayBuffer
import awesomeware.core.entities.GameEntity

abstract class CommandResult(val count: Integer, val command: Command, val output: Seq[Any])
case class CommandSuccess(override val count: Integer, override val command: Command, override val output: Seq[Any]) extends CommandResult(count,command, output)
case class CommandFailure(override val count: Integer, override val command: Command, override val output: Seq[Any]) extends CommandResult(count,command, output)
case class NoCommand() extends CommandResult(0, null, null)

object CommandInterpreter {
  def interpret(text: String, source: GameEntity, commands: Set[Command]):CommandResult = {
    val tokens = text.split(" ")
    val input = ParseState(text, tokens, 0)
    var bestEffort:CommandResult = null
    
    for (command <- commands) {
      val res:CommandResult = command.parseInput(input, source)
      if(bestEffort == null) {
        if(res.count > 0)
        	bestEffort = res
      } else {
	      bestEffort match {
	        case c:CommandSuccess =>
	          if(res.count > bestEffort.count && res.isInstanceOf[CommandSuccess]) {
	            bestEffort = res
	          }
	        case c:CommandFailure =>
	          if(res.isInstanceOf[CommandSuccess]) {
	        	  bestEffort = res
	          } else if(res.count > bestEffort.count) {
	        	  bestEffort = res
	          }
	      }
      }
    }
    
    if(bestEffort == null) return NoCommand()
    
    if(bestEffort.isInstanceOf[CommandSuccess]) {
      bestEffort.command.go(source, bestEffort.output)
    }
    return bestEffort
  }
}