package awesomeware.core.entities

import awesomeware.core.DescType._
import awesomeware.core.Container

class Mob extends GameEntity with Container {
  def describeTo(to: GameEntity, dType: DescType):String = {
    dType match {
      case ShortDesc =>
        return shortDesc(to)
      case LongDesc =>
        return longDesc(to)
    }
  }
  
  def shortDesc(to: GameEntity): String = {
    return "mob.short"
  }
  
  def longDesc(to: GameEntity): String = {
    return "mob.long"
  }
}