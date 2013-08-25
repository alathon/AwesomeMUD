package com.awesomeware.commands

import scala.collection.mutable.ArrayBuffer
import com.awesomeware.core.entities.GameEntity

case class ParseState(text: String, tokens: Seq[String], offset: Int)

abstract sealed class Result[+T] {
  val success: Boolean

  def getResult: T

  def input: ParseState
}

case class Success[+T](result: T, in: ParseState) extends Result[T] {
  def getResult: T = result

  def input: ParseState = in

  override val success = true
}

case class Failure[+T](msg: String, in: ParseState) extends Result[T] {
  override val success = false

  def input: ParseState = in

  def getResult: Nothing = scala.sys.error("Tried to get result from failure.")
}

abstract class Command {
  val components: Seq[CommandComponent[_]]
  val name: String

  implicit def string2option(s: String): Option[String] = {
    s match {
      case "" => None
      case _ => Some(s)
    }
  }

  def go(source: GameEntity, args: Seq[Any])

  def parseInput(in: ParseState, source: GameEntity): CommandResult = {
    val out = ArrayBuffer[Any]()
    var currentInput = in
    var componentsMatched = 0
    for (component <- components) {
      if (currentInput.offset >= currentInput.tokens.length) {
        if (!component.optional) {
          return ParseFailure(componentsMatched, this, out)
        }
      } else {
        val res = component.matchInput(currentInput, source)
        if (!res.success) {
          if (!component.optional) {
            return ParseFailure(componentsMatched, this, out)
          }
        } else {
          componentsMatched += 1
          if (component.shouldAdd) {
            out += res.getResult
          }
        }
        currentInput = res.asInstanceOf[Result[_]].input
      }
    }

    ParseSuccess(componentsMatched, this, out)
  }
}