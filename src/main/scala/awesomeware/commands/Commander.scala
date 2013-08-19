package awesomeware.commands

import awesomeware.core.entities.GameEntity

trait Commander {
  def parseCommand(text: String): (Boolean, Command) {
    // TODO: Propagate through CommandINterpreter
    // so that it returns a (Boolean, Command) set
    // according to which command was the best effort.
    // null Command means no appropriate command was found.
    // These could also be case classes instead.. probably
    // a better idea so you can return NoValidCommand() vs.
    // IncorrectSyntax() case classes
    val (ok, cmd) = CommandInterpreter.interpret(text, this.getCommandSource(), this.commands)
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