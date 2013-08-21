package awesomeware.core

import awesomeware.core.entities.GameEntity

trait Container {
  var inventory: List[GameEntity] = List()

  def canEnter(obj: GameEntity, from: Container): Boolean = true

  def canExit(obj: GameEntity, to: Container): Boolean = true

  def entered(obj: GameEntity, from: Container) {

  }

  def exited(obj: GameEntity, to: Container) {

  }

  def removeEntity[T <: GameEntity](e: T) {
    this.inventory = this.inventory diff List(e)
  }

  def addEntity[T <: GameEntity](e: T) {
    this.inventory = e :: this.inventory
  }
}