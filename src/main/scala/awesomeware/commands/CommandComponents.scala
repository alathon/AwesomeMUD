package awesomeware.commands

import awesomeware.core.Container
import awesomeware.core.entities.GameEntity
import awesomeware.core.entities.Mob
import awesomeware.core.entities.Room

abstract class CommandComponent[T](val optional: Boolean = false) {
  def shouldAdd: Boolean
  def matchInput(in: ParseState, source: GameEntity): Result[T]
}

object Search {
  private def getEntitiesInLocation(locName: String, source: GameEntity): Seq[GameEntity] = {
    locName.toLowerCase() match {
      case "players" =>
        Seq[GameEntity]()
      case "here" =>
        source.location.inventory
      case "self" =>
        source match {
          case c:Container =>
            c.inventory
          case _ =>
            Seq[GameEntity]()
        }
    }
  }

  private def splitByNumber(str: String): (String, Integer) = {
    val pattern = """(\d+)\.(.+)""".r
    val matches = pattern.findAllIn(str).matchData.toList

    matches.isEmpty match {
      case true => (str, 0)
      case false => (matches(0).group(2), matches(0).group(1).toInt)
    }
  }

  private def isType(e: GameEntity, typeName: Option[String]): Boolean = {
    typeName match {
      case Some(a) =>
        a match {
          case "mob" =>
            e.isInstanceOf[Mob]
          case "room" =>
            e.isInstanceOf[Room]
        }
      case None =>
        true
    }
  }

  def find(source: GameEntity, name: String, typeName: Option[String], locName: String): Option[GameEntity] = {
    val (newName, number) = this.splitByNumber(name)
    val entities: Seq[GameEntity] = this.getEntitiesInLocation(locName, source).filter(x => isType(x, typeName))

    if (entities.isEmpty || number > entities.length - 1) {
      return None
    } else {
      return Some(entities(number))
    }
  }
}

class Ref(typeName: Option[String] = None, locName: String = "here", optional: Boolean = false)
  extends CommandComponent[GameEntity](optional) {
  def shouldAdd: Boolean = true

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

class Anything(optional: Boolean = false) extends CommandComponent[String](optional) {
  def shouldAdd: Boolean = true

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    Success(in.tokens.slice(in.offset, in.tokens.length + 1).mkString(" "), in.copy(offset = in.tokens.length))
  }
}

class Word(word: Option[String], optional: Boolean = false, force: Boolean = false, short: Boolean = false)
  extends CommandComponent[String](optional) {

  def this(word: Option[String]) = this(word, false, false, false)

  def this() = this(None, false, false, false)

  def shouldAdd: Boolean = force || false

  override def toString: String = s"Word: $word Optional: $optional Force:$force Short:$short"

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    val t = in.tokens(in.offset)
    word match {
      case None =>
        Success(t, in.copy(offset = in.offset + 1))
      case Some(e) =>
        if((short && e.startsWith(t)) || e == t)
            Success(t, in.copy(offset = in.offset + 1))
        else
            Failure(s"Expected word: $e Got: $t", in)
    }
  }
}

class Number(optional: Boolean = false)
  extends CommandComponent[Integer](optional) {

  def shouldAdd: Boolean = true

  def matchInput(in: ParseState, source: GameEntity): Result[Integer] = {
    val t = in.tokens(in.offset)
    t forall Character.isDigit match {
      case true => Success(t.toInt, in.copy(offset = in.offset + 1))
      case false => Failure("Not a number.", in)
    }
  }
}

class Or[T](components: Seq[CommandComponent[T]], opt: Boolean = false)
  extends CommandComponent[T](opt) {
  def this(components: Seq[CommandComponent[T]]) = this(components, false)

  def shouldAdd: Boolean = components.exists (_.shouldAdd)

  def matchInput(in: ParseState, source: GameEntity): Result[T] = {
    components.forall(x => x.matchInput(in, source).success)
    for (comp <- components) {
      val res = comp.matchInput(in, source)
      if (res.success) {
        return res
      }
    }
    Failure("No match for Or", in)
  }
}