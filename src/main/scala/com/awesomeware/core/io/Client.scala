package com.awesomeware.core.io

import akka.actor._
import java.net.InetSocketAddress
import akka.util.ByteString
import akka.io.Tcp
import com.awesomeware.core.entities.{Mob, GameEntity}
import com.awesomeware.commands.{NoCommand, ParseFailure, ParseSuccess, Commander}
import com.awesomeware.core.{MSDPMessages, Telnet, MSDP, World}
import com.awesomeware.commands.impl.{CommunicationCommands, MovementCommands, BasicUtilityCommands}
import com.awesomeware.content.staticContent.TheVoid
import com.awesomeware.core.TestMsgs

object Client {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new Client(remote, connection))
}

class Client(remote: InetSocketAddress, connection: ActorRef)
  extends Actor with ActorLogging with Commander {

  def write(s: ByteString) {
    connection ! Tcp.Write(s)
  }

  def setMSDP(active: Boolean) {
    this.msdpActive = active
    if (active) {
      this.receiveText("MSDP activated.")
    } else {
      this.receiveText("MSDP deactivated.")
    }
  }

  def handleTelnetDont(doType: Byte) = doType match {
    case Telnet.MSDP =>
      this.setMSDP(false)
    case _ =>
      log.error("Unhandled IAC DONT: " + doType)
  }
  
  def handleTelnetDo(doType: Byte) = doType match {
    case Telnet.MSDP =>
      this.setMSDP(true)
    case _ =>
      log.error("Unhandled IAC DO: " + doType)
  }
  
  def handleMsdpData(body: List[Byte]) {
	  val msg = MSDP.parseData(body)
	  log.info("Got message: " + msg)
  }
  
  def handleTelnetSendByte(protocol: Byte, xs: List[Byte]): List[Byte] = {
    val body = xs.takeWhile(_ != Telnet.IAC)
    val afterBody = xs(body.length+1)
    
    afterBody match {
      case Telnet.SE =>
        protocol match {
          case Telnet.MSDP =>
            handleMsdpData(body)
          case _ =>
            log.error("Unhandled protocol: " + protocol)
        }
        xs.drop(body.length+2) // Drop the IAC and the SE bytes.

      case _ =>
        log.error("Something wrong with byte stream: " + xs)
        List[Byte]()
    }
  }

  def test(bs: List[Byte]): List[Byte] = bs match {
    case Nil =>
      List()

    case Telnet.IAC :: Telnet.DO :: x :: xs =>
      handleTelnetDo(x)
      test(xs)
      
    case Telnet.IAC :: Telnet.DONT :: x :: xs =>
      handleTelnetDont(x)
      test(xs)
   
    case Telnet.IAC :: Telnet.SB :: x :: xs =>
      val rest = handleTelnetSendByte(x, xs)
      test(rest)

    case _ =>
      log.error("Unsupported Telnet.IAC case: " + bs)
      List()
  }
    
  def handleIAC(data: ByteString) {
    val b = new StringBuilder()
    for (byte <- data) {
      b ++= byte.toString
      b ++= " "
    }
    log.info("Bytes received: " + b.toString())
    
    test(data.toList)
  }

  def receive: Receive = {
    case Tcp.Received(data: ByteString) =>
      if (data(0) == Telnet.IAC) {
        this.handleIAC(data)
      } else {
        val text = data.utf8String.replaceAll("[\r\n]+$", "")
        this.handleInput(text)
        log.info("Received {} from remote address {}", text, remote)
      }
    case _: Tcp.ConnectionClosed =>
      log.info("Stopping, because connection for remote address {} closed", remote)
      removeFromGame()
      context.stop(self)
    case Terminated(`connection`) =>
      log.info("Stopping, because connection for remote address {} died", remote)
      removeFromGame()
      context.stop(self)
    case s =>
      log.info("Received " + s)
  }

  context.watch(connection)

  def removeFromGame() {
    World.clients -= Client.this
    player = null
  }

  def receiveText(str: String, prompt: Boolean = true, color: Boolean = true, newline: Boolean = true) {
    val b = new StringBuilder()
    b ++= str
    if (newline) b ++= "\r\n"

    if (prompt) {
      if (newline) b ++= "> "
      else b ++= "\r\n> "
    }

    if (color)
      this.write(ByteString(Color.colorize(b.toString())))
    else
      this.write(ByteString(Color.colorize(b.toString(), keep = false)))
  }

  def getCommandSource[S <: GameEntity]: S = this.player.asInstanceOf[S]

  def handleInput(text: String) {
    inputGrabber.active match {
      case true =>
        inputGrabber.receive(text, Client.this)
      case false =>
        text.trim() match {
          case "" =>
            receiveText("")
          case _ =>
            parseCommand(text) match {
              case ParseSuccess(_, cmd, out) =>
                cmd.go(this.player, out)
              case ParseFailure(_, cmd, _) =>
                this.receiveText(s"Invalid syntax for command: ${cmd.name}")
              case NoCommand() =>
                this.receiveText(s"No such command: $text")
            }
        }
    }
  }


  var msdpActive: Boolean = false
  val inputGrabber = new InputGrabber()

  // Try and enable MSDP
  //this.write(MSDPMessages.willMSDP)

  this.handleIAC(TestMsgs.clientDo)
  this.handleIAC(TestMsgs.clientDont)
  this.handleIAC(TestMsgs.msdpVar)
  /**
   * Login stuff.
   */
  World.clients += Client.this
  BasicUtilityCommands.giveAll(this)
  MovementCommands.giveAll(this)
  CommunicationCommands.giveAll(this)

  var player: Mob = new Mob()
  player.client = Some(this)
  player.name = "Named"
  player.move(TheVoid, None)
}