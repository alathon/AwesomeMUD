package awesomeware.core.entities

import awesomeware.core.io.Client
import awesomeware.core.DescType._
import awesomeware.core.Container

/**
 * Base class for all entities in the game world.
 * Entities have a location (inside a Container),
 * and can Move to a different container if they're allowed.
 * Leaving will trigger Left() after you've left, and Entered()
 * after you've entered a new Container.
 */
abstract class GameEntity {
  var location: Container = null
  var client: Option[Client] = None
  var name: String = "Unnamed Entity"

  def receiveText(text: String, prompt: Boolean = true, newline: Boolean = true) {
    client match {
      case Some(c: Client) =>
        c.receiveText(text, prompt, newline)
      case None =>
    }
  }

  def describeTo(to: GameEntity, dtype: DescType): String

  def move(to: Container): Boolean = {
    if (this.location != null && !this.location.canExit(this, to)) {
      return false
    }

    if (!to.canEnter(this, this.location)) {
      return false
    }

    val oldLoc = this.location
    this.location = to

    to.addEntity(this)
    if (oldLoc != null) {
      oldLoc.removeEntity(this)
      oldLoc.exited(this, to)
    }
    to.entered(this, oldLoc)
    return true
  }
}