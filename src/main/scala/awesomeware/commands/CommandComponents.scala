package awesomeware.commands

import awesomeware.core.Container
import awesomeware.core.entities.GameEntity
import awesomeware.core.entities.Mob
import awesomeware.core.entities.Room
import scala.collection.mutable.ArrayBuffer

abstract class CommandComponent[T](val optional:Boolean = false){
	def shouldAdd:Boolean
	def matchInput(in: ParseState, source: GameEntity): Result[T]
}

object Search {
  def getEntitiesInLocation(locName: String, source: GameEntity): Seq[GameEntity] = {
    locName.toLowerCase() match {
      case "players" => 
        return Seq[GameEntity]()
      case "here" =>
        return source.location.inventory
      case "self" =>
        if(source.isInstanceOf[Container]) {
          return source.asInstanceOf[Container].inventory
        } else {
          return Seq[GameEntity]()
        }
    }
  }
  
  def splitByNumber(str: String): (String, Integer) = {
	  val pattern = """(\d+)\.(.+)""".r
	  val matches = pattern.findAllIn(str).matchData.toList
	  if(matches.isEmpty) {
	    return (str, 0)
	  } else {
	    return (matches(0).group(2), matches(0).group(1).toInt)
	  }
  }

  def find(source: GameEntity, name: String, typeName: String, locName: String): Option[GameEntity] = {
    val (newName, number) = this.splitByNumber(name)
    val entities:Seq[GameEntity] = this.getEntitiesInLocation(locName, source)
    									.filter(x => isType(x,typeName))
    if(entities.isEmpty || number > entities.length-1) {
      return None
    } else {
    	return Some(entities(number))
    }
  }
  
  def isType(e: GameEntity, typeName: String):Boolean = {
    if(typeName == null || typeName == "") return true
    
	  typeName match {
	    case "mob" =>
	      return e.isInstanceOf[Mob]
	    case "room" =>
	      return e.isInstanceOf[Room]
	  }
  }
}

class Ref(typeName: String = "", locName: String = "here", optional:Boolean = false)
	extends CommandComponent[GameEntity](optional) {
	def shouldAdd:Boolean = true
	def matchInput(in: ParseState, source: GameEntity): Result[GameEntity] = {
		val t = in.tokens(in.offset)
		Search.find(source, t, typeName, locName) match {
		  case Some(e) =>
		    return Success(e, in.copy(offset = in.offset + 1))
		  case None =>
		    return Failure(s"No such entity as $t", in)
		}
	}
}

class Anything(optional:Boolean = false) extends CommandComponent[String](optional) {
  def apply():Anything = new Anything()

  def shouldAdd:Boolean = true

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    val tokens = in.tokens.slice(in.offset, in.tokens.length+1)
    return Success(tokens.mkString(" "), in.copy(offset = in.tokens.length))
  }
}

class Word(word: String, optional:Boolean = false, force:Boolean = false, short:Boolean = false)
	extends CommandComponent[String](optional) {
  def this(word: String) = this(word,false,false,false)
  def this() = this("",false,false,false)
  
  def shouldAdd:Boolean = force || false

  override def toString:String = s"Word: $word Optional: $optional Force:$force Short:$short"
  
  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    val t = in.tokens(in.offset)
    
    if(word == "") {
      return Success(t, in.copy(offset = in.offset + 1))
    }
    
    val diff = word.length() - t.length()
    var wordComp = word
    if(short && diff > 0) {
      wordComp = word.substring(0, word.length() - diff)
    }

    if(t == wordComp) {
      return Success(t, in.copy(offset = in.offset + 1))
    } else {
      return Failure(s"Expected word: $word Got: $t", in)
    }
  }
}

class Number(optional:Boolean = false) 
	extends CommandComponent[Integer](optional) {
  def shouldAdd:Boolean = true
  def isAllDigits(x: String) = x forall Character.isDigit
  def matchInput(in: ParseState, source: GameEntity): Result[Integer] = {
		val t = in.tokens(in.offset)
		if(t forall Character.isDigit) {
		  Success(t.toInt, in.copy(offset = in.offset + 1))
		} else {
		  Failure("Not a number.", in)
		}
  }
}

class Or[T](components: Seq[CommandComponent[T]], opt:Boolean = false) 
	extends CommandComponent[T](opt) {
  def this(components: Seq[CommandComponent[T]]) = this(components,false)
  
	def shouldAdd:Boolean = {
	  for(comp <- components) {
	    if(comp.shouldAdd) return true
	  }
	  return false
	}

	def matchInput(in: ParseState, source: GameEntity): Result[T] = {
	  for(comp <- components) {
	    val res = comp.matchInput(in, source)
	    if(res.success) {
	    	return res
	    }
	  }
	  Failure("No match for Or", in)
	}
}