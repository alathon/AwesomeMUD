package com.awesomeware.core

import akka.util.ByteString
import scala.Predef._
import scala.Some
import com.awesomeware.core.io.Client

sealed abstract class MSDPMessage

sealed abstract class MSDPValue

case class MSDPArray(values: List[String]) extends MSDPValue

case class MSDPTable(values: Map[String, String]) extends MSDPValue

case class MSDPVar(value: String) extends MSDPValue

case class MSDPOutput(varName: String, varValue: MSDPValue) extends MSDPMessage

case object MSDPDo extends MSDPMessage

case object MSDPDont extends MSDPMessage

// For debug purposes...
object TestMsgs {
  val clientDo = MSDP.message(Telnet.IAC, Telnet.DO, Telnet.MSDP)

  val clientDont = MSDP.message(Telnet.IAC, Telnet.DONT, Telnet.MSDP)

  val msdpVar = MSDP.message(Telnet.IAC, Telnet.SB, Telnet.MSDP,
    Telnet.MSDP_VAR, 110.toByte, 97.toByte, 109.toByte, 101.toByte,
    Telnet.MSDP_VAL, 118.toByte, 97.toByte, 108.toByte,
    Telnet.IAC, Telnet.SE)

  val msdpArray = MSDP.message(Telnet.IAC, Telnet.SB, Telnet.MSDP,
    Telnet.MSDP_VAR, 110.toByte, 97.toByte, 109.toByte, 101.toByte,
    Telnet.MSDP_VAL, Telnet.MSDP_ARRAY_OPEN,
    Telnet.MSDP_VAL, 111.toByte, 110.toByte, 101.toByte,
    Telnet.MSDP_VAL, 116.toByte, 119.toByte, 111.toByte,
    Telnet.MSDP_ARRAY_CLOSE, Telnet.IAC, Telnet.SE)

  val msdpTable = MSDP.message(Telnet.IAC, Telnet.SB, Telnet.MSDP,
    Telnet.MSDP_VAR, 110.toByte, 97.toByte, 109.toByte, 101.toByte,
    Telnet.MSDP_VAL, Telnet.MSDP_TABLE_OPEN,

    Telnet.MSDP_VAR, 116.toByte, 49.toByte,
    Telnet.MSDP_VAL, 118.toByte, 49.toByte,

    Telnet.MSDP_VAR, 116.toByte, 50.toByte,
    Telnet.MSDP_VAL, 118.toByte, 50.toByte,
    Telnet.MSDP_TABLE_CLOSE, Telnet.IAC, Telnet.SE)

  val msdpTableNoClose = MSDP.message(Telnet.IAC, Telnet.SB, Telnet.MSDP,
    Telnet.MSDP_VAR, 110.toByte, 97.toByte, 109.toByte, 101.toByte,
    Telnet.MSDP_VAL, Telnet.MSDP_TABLE_OPEN,

    Telnet.MSDP_VAR, 116.toByte, 49.toByte,
    Telnet.MSDP_VAL, 118.toByte, 49.toByte,

    Telnet.MSDP_VAR, 116.toByte, 50.toByte,
    Telnet.MSDP_VAL, 118.toByte, 50.toByte,
    Telnet.IAC, Telnet.SE)

  val msdpTableNoOpen = MSDP.message(Telnet.IAC, Telnet.SB, Telnet.MSDP,
    Telnet.MSDP_VAR, 110.toByte, 97.toByte, 109.toByte, 101.toByte,
    Telnet.MSDP_VAL,

    Telnet.MSDP_VAR, 116.toByte, 49.toByte,
    Telnet.MSDP_VAL, 118.toByte, 49.toByte,

    Telnet.MSDP_VAR, 116.toByte, 50.toByte,
    Telnet.MSDP_VAL, 118.toByte, 50.toByte,
    Telnet.MSDP_TABLE_CLOSE, Telnet.IAC, Telnet.SE)
}

object MSDPMessages {
  val willMSDP = MSDP.message(Telnet.IAC, Telnet.WILL, Telnet.MSDP)
}

object MSDP {
  def message(bs: Byte*): ByteString = {
    bs.foldLeft(ByteString())((r, c) => r ++ ByteString(c))
  }

  // Data has had MSDP_TABLE_OPEN and MSDP_TABLE_CLOSE removed.
  // So everything inside should be MSDP_VAR "Name" MSDP_VAL "Value" quads
  def parseTable(bs: List[Byte]): Option[MSDPTable] = {
    def inner(bs: List[Byte]): Map[String, String] = bs match {
      case Nil =>
        Map[String, String]()
      // Step 1: Match an MSDP_VAR "String" pair.
      case Telnet.MSDP_VAR :: es =>
        es.takeWhile(_ != Telnet.MSDP_VAL) match {
          case Nil =>
            null
          case bytes =>
            val varName = ByteString(bytes.toArray).utf8String
            val after = es.drop(bytes.length)
            // Step 2: Match an MSDP_VAL "String" pair
            after match {
              case Nil =>
                null

              case Telnet.MSDP_VAL :: xs =>
                xs.takeWhile(_ != Telnet.MSDP_VAR) match {
                  case Nil => null
                  case bytes2 =>
                    val varValue = ByteString(bytes2.toArray).utf8String
                    Map[String, String](varName -> varValue) ++ inner(xs.drop(bytes.length))
                }
                
              case e =>
                println(s"Serious issue: $e not covered!")
                null
            }
        }
        
      case e =>
        println(s"Serious issue: $e not covered!")
        null
    }

    inner(bs) match {
      case null =>
        None
      case m: Map[String, String] if m.isEmpty =>
        None
      case m: Map[String, String] =>
        Some(MSDPTable(m))
    }
  }

  // Data has had MSDP_ARRAY_OPEN and MSDP_ARRAY_CLOSE removed.
  // So everything inside should be MSDP_VAL "String" MSDP_VAL "String" ...
  def parseArray(bs: List[Byte]): Option[MSDPArray] = {
    def inner(bs: List[Byte]): List[String] = bs match {
      case Nil => List[String]()
      case e :: es if e == Telnet.MSDP_VAL =>
        es.takeWhile(_ != Telnet.MSDP_VAL) match {
          case Nil => Nil
          case bytes =>
            ByteString(bytes.toArray).utf8String :: inner(es.drop(bytes.length))
        }
    }

    inner(bs) match {
      case Nil =>
        None
      case l: List[String] =>
        Some(MSDPArray(l))
    }
  }

  def parseData(bs: List[Byte]): Option[MSDPMessage] = bs match {
    // Data is of the format MSDP_VAR <VARIABLE> MSDP_VAL <VALUE>
    // where <VALUE> can either be a table, array, or a string.
    case Telnet.MSDP_VAR :: xs =>
	    val varBytes = xs.takeWhile(_ != Telnet.MSDP_VAL)
	    val after = xs.drop(varBytes.length)
	    val varName = ByteString(varBytes.toArray).utf8String
	
	    after match {
	      case Telnet.MSDP_VAL :: Telnet.MSDP_ARRAY_OPEN :: ys =>
	        ys.last match {
	          case Telnet.MSDP_ARRAY_CLOSE =>
	            val data = ys.dropRight(1)
	            parseArray(data) match {
	              case None => None
	              case Some(array: MSDPArray) => Some(MSDPOutput(varName, array))
	            }
	            
	          case _ => None
	        }
	
	      case Telnet.MSDP_VAL :: Telnet.MSDP_TABLE_OPEN :: ys =>
	        ys.last match {
	          case Telnet.MSDP_TABLE_CLOSE =>
	            val data = ys.dropRight(1)
	            parseTable(data) match {
	              case None => None
	              case Some(table: MSDPTable) => Some(MSDPOutput(varName, table))
	            }
	          case _ => None
	        }

	      case Telnet.MSDP_VAL :: ys =>
	        val varValue = ByteString(ys.toArray).utf8String
	        Some(MSDPOutput(varName, MSDPVar(varValue)))
	        
	      case _ => 
	        None
	    }
    case _ =>
      None
    
  }
}

object Telnet {
  val mapping: Map[Byte, String] = Map[Byte, String](IAC -> "IAC", DONT -> "DONT", DO -> "DO",
    WILL -> "WILL", WONT -> "WONT", SB -> "SB", SE -> "SE",

    MSDP -> "MSDP", MSDP_VAR -> "MSDP_VAR", MSDP_VAL -> "MSDP_VAL", MSDP_TABLE_OPEN -> "MSDP_TABLE_OPEN",
    MSDP_TABLE_CLOSE -> "MSDP_TABLE_CLOSE", MSDP_ARRAY_OPEN -> "MSDP_ARRAY_OPEN", MSDP_ARRAY_CLOSE -> "MSDP_ARRAY_CLOSE")

  val IAC = 255.toByte
  val DONT = 254.toByte
  val DO = 253.toByte
  val WILL = 251.toByte
  val WONT = 252.toByte
  val SB = 250.toByte
  val SE = 240.toByte


  val MSDP = 69.toByte
  val MSDP_VAR = 1.toByte
  val MSDP_VAL = 2.toByte
  val MSDP_TABLE_OPEN = 3.toByte
  val MSDP_TABLE_CLOSE = 4.toByte
  val MSDP_ARRAY_OPEN = 5.toByte
  val MSDP_ARRAY_CLOSE = 6.toByte
}