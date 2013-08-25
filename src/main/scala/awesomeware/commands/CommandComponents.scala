package awesomeware.commands

import awesomeware.core.{World, Container}
import awesomeware.core.entities.{GameEntity, Mob, Room}

abstract class CommandComponent[T](val optional: Boolean = false) {
  def shouldAdd: Boolean

  def matchInput(in: ParseState, source: GameEntity): Result[T]
}

object Search {
  private def getEntitiesInLocation[T >: GameEntity](locName: String, source: GameEntity): Seq[T] = {
    locName.toLowerCase match {
      case "players" =>
        World.clients.map(x => x.player).toList
      case "here" =>
        source.location.inventory
      case "self" =>
        source match {
          case c: Container =>
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
    val entities: Seq[GameEntity] = this.getEntitiesInLocation(locName, source).filter(x => isType(x, typeName) && x.name == newName)

    if (entities.isEmpty || number > entities.length - 1) {
      None
    } else {
      Some(entities(number))
    }
  }
}

object Ref {
  def apply(typeName: Option[String] = None, locName: String = "here", optional: Boolean = false): Ref =
    new Ref(typeName, locName, optional)
}

class Ref(typeName: Option[String] = None, locName: String = "here", optional: Boolean = false)
  extends CommandComponent[GameEntity](optional) {
  def shouldAdd: Boolean = true

  def matchInput(in: ParseState, source: GameEntity): Result[GameEntity] = {
    val t = in.tokens(in.offset)
    Search.find(source, t, typeName, locName) match {
      case Some(e) =>
        Success(e, in.copy(offset = in.offset + 1))
      case None =>
        Failure(s"No such entity as $t", in)
    }
  }
}

object Anything {
  def apply(optional: Boolean = false): Anything = new Anything(optional)
}

class Anything(optional: Boolean = false) extends CommandComponent[String](optional) {
  def shouldAdd: Boolean = true

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    Success(in.tokens.slice(in.offset, in.tokens.length + 1).mkString(" "), in.copy(offset = in.tokens.length))
  }
}

object Word {
  def apply(word: Option[String] = None, optional: Boolean = false, force: Boolean = false, short: Boolean = false): Word = {
    new Word(word, optional, force, short)
  }
}

class Word(word: Option[String], optional: Boolean = false, addToOutput: Boolean = false, short: Boolean = false)
  extends CommandComponent[String](optional) {

  def shouldAdd: Boolean = addToOutput

  override def toString: String = s"Word: $word Optional: $optional Force:$addToOutput Short:$short"

  def matchInput(in: ParseState, source: GameEntity): Result[String] = {
    val t = in.tokens(in.offset)
    word match {
      case None =>
        Success(t, in.copy(offset = in.offset + 1))
      case Some(e) =>
        if ((short && e.startsWith(t)) || e == t)
          Success(t, in.copy(offset = in.offset + 1))
        else
          Failure(s"Expected word: $e Got: $t", in)
    }
  }
}

object Number {
  def apply(optional: Boolean = false): Number = {
    new Number(optional)
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

object Or {
  def apply[T](components: CommandComponent[T]*): Or[T] = {
    new Or[T](false, components)
  }


  def apply[T](optional: Boolean, components: CommandComponent[T]*): Or[T] = {
    new Or[T](optional, components)
  }
}

class Or[T](optional: Boolean = false, components: Seq[CommandComponent[T]])
  extends CommandComponent[T](optional) {

  def shouldAdd: Boolean = components.exists(_.shouldAdd)

  def matchInput(in: ParseState, source: GameEntity): Result[T] = {
    components.view map {
      x => x.matchInput(in, source)
    } find (_.success) match {
      case Some(r) => r
      case None => Failure("Failed to match", in)
    }
  }
}