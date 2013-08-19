package awesomeware.commands

import awesomeware.core.entities.GameEntity

abstract class CommandBatch {
	val commands:List[Command]
	val name:String
	
	def giftPossibleCommands(entity: GameEntity)
	def removeCommands(entity: GameEntity)
}