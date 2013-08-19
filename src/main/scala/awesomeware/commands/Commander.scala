package awesomeware.commands

import awesomeware.core.entities.GameEntity

trait Commander {
  def handleCommand(text: String) {
    val ok = CommandInterpreter.interpret(text, this.getCommandSource(), this.commands)
  }

  def getCommandSource[S <: GameEntity]():S
  
  var commands = Set[Command]()
  
  def addCommand(cmd: Command) {
    this.commands = this.commands + cmd
  }
  
  def addCommands(cmds: Set[Command]) {
	  this.commands = this.commands union cmds
  }
  
  def removeCommands(cmds: Set[Command]) {
	  this.commands = this.commands -- cmds
  }
}