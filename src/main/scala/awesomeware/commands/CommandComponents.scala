package awesomeware.commands

import awesomeware.core.entities.GameEntity

abstract class CommandComponent[T](val optional:Boolean = false){
	def shouldAdd:Boolean
	def matchInput(in: ParseState, source: GameEntity): Result[T]
}

class Word(word: String, optional:Boolean = false, force:Boolean = false, short:Boolean = false)
	extends CommandComponent[String](optional) {
  def this(word: String) = this(word,false,false,false)
  def this() = this("",false,false,false)
  def shouldAdd:Boolean = force || false

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    val t = in.tokens(in.offset)
    
    if(word == "") {
      Success(t, in.copy(offset = in.offset + 1))
    }
    
    val diff = word.length() - t.length()
    var wordComp = word
    if(short && diff > 0) {
      wordComp = word.substring(0, word.length() - diff)
    }

    if(t == wordComp) {
      Success(t, in.copy(offset = in.offset + 1))
    } else {
      Failure(s"Expected word: $word Got: $t", in)
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