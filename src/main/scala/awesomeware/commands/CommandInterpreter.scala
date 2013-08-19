package awesomeware.commands

import scala.collection.mutable.ArrayBuffer
import awesomeware.core.entities.GameEntity

object CommandInterpreter {
  def interpret(text: String, source: GameEntity, commands: Set[Command]):Boolean = {
    val tokens = text.split(" ")
    val input = ParseState(text, tokens, 0)
    
    for (command <- commands) {
      if(tryCommand(command, input, source)) return true
    }
    return false
  }
  
  def tryCommand(command: Command, input: ParseState, source: GameEntity): Boolean = {
    val (success, output) = command.parseInput(input, source)
    if(!success) {
      return false
    } else {
    	command.go(source, output)
    	return true
    }
  }
}