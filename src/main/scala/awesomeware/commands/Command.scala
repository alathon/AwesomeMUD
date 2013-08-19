package awesomeware.commands

import awesomeware.core.GameEntity
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

sealed case class ParserInput(text: String, tokens: List[String], source: GameEntity, offset: Integer)

sealed abstract class Result[+T] {
  def map[U](f: T => U): Result[U]
  def mapPartial[U](f: PartialFunction[T, U], error: T => String): Result[U]
  def flatMapWithNext[U](f: T => ParserInput => Result[U]): Result[U]
  def append[U >: T](a: => Result[U]): Result[U]
}

case class Success[+T](result:T, next:ParserInput) extends Result {
  def map[U](f: T => U) = Success(f(result), next)
  def mapPartial[U](f: PartialFunction[T, U], error: T => String): Result[U]
    = if(f.isDefinedAt(result)) Success(f(result), next)
      else Failure(error(result), next)

  def flatMapWithNext[U](f: T => ParserInput => Result[U]):  Result[U]
    = f(result)(next)
  def append[U >: T](a: => Result[U]):  Result[U] = this
}
case class Failure[+T](message:String, next:ParserInput) extends Result

abstract class Parser[+T] extends (ParserInput => Result[T]) {
  def append[U >: T](p: => Parser[U]): Parser[U]

  def Parser[T](f: ParserInput => Result[T]): Parser[T]
    = new Parser[T]{ def apply(in: ParserInput) = f(in) }
  def apply(in: ParserInput): Result[T]

  def map[U](f: T => U): Parser[U]
    = Parser{ in => this(in) map(f)}
  def ~[U](p: Parser[U]): Parser[~[T,U]] = (for(a <- this; b <- p) yield new ~(a,b)).named("~")
}


class NumberParser extends Parser[Integer] {
  def apply(in: ParserInput): Result[Integer] = {
    val curToken = in.tokens(in.offset)
    if(curToken forall Character.isDigit) {
      Success(curToken.toInt, in.copy(offset = in.offset + 1))
    } else {
      Failure("Not a number", in)
    }
  }
}

class WordParser extends Parser[String] {
  def apply(in: ParserInput): Result[String] = {
    val curToken = in.tokens(in.offset)
    Success(curToken, in.copy(offset = in.offset + 1))
  }
}