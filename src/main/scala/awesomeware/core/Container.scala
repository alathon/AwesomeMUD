package awesomeware.core

import awesomeware.core.entities.GameEntity

trait Container {
  var inventory:List[GameEntity] = List()

  def canEnter(obj: GameEntity, from: Container):Boolean = true

  def canExit(obj: GameEntity, to: Container):Boolean = true

  def entered(obj: GameEntity, from: Container) {
    println(obj + " entered from " + from)
    for (e <- inventory) {
      println("In inventory: " + e)
    }
  }

  def exited(obj: GameEntity, to: Container) {
    println(obj + " exited to " + to)
  }

  def removeEntity[T <: GameEntity](e: T) {
    this.inventory = this.inventory diff List(e)
  }

  def addEntity[T <: GameEntity](e: T) {
    this.inventory = e :: this.inventory
  }
}