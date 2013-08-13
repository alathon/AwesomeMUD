package awesomeware.core

import akka.actor.{ActorLogging, Actor}
import awesomeware.core.io.Client

/**
 * Base class for all entities in the game world.
 * Entities have a location (inside a Container),
 * and can Move to a different container if they're allowed.
 * Leaving will trigger Left() after you've left, and Entered()
 * after you've entered a new Container.
 */
abstract class GameEntity{
  var location: Container = null
  var client:Client = null

  def Move(to: Container):Boolean = {
    if(this.location != null && !this.location.canExit(this, to)) {
      return false
    }

    if(!to.canEnter(this, this.location))  {
      return false
    }

    val oldLoc = this.location
    this.location = to

    to.addEntity(this)
    if(oldLoc != null) {
      oldLoc.removeEntity(this)
      oldLoc.Exited(this, to)
    }
    to.Entered(this, oldLoc)
    true
  }

  def Entered(to: Container, from: Container) {

  }
  def Exited(from: Container) {

  }
}