package awesomeware.core.io

import akka.actor._
import java.net.{InetSocketAddress, Socket}
import akka.util.ByteString
import akka.io.Tcp
import awesomeware.core.entities.Mob
import scala.collection.mutable.ArrayBuffer
import awesomeware.content.staticContent._
import awesomeware.core.entities.GameEntity
import awesomeware.commands._
import awesomeware.commands.impl._

object Client {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new Client(remote, connection))
}

class Client(remote:InetSocketAddress, connection:ActorRef) 
	extends Actor with ActorLogging with Commander {

  def write(s: ByteString) {
    connection ! Tcp.Write(s)
  }

  def receive: Receive = {
    case Tcp.Received(data: ByteString) =>
      val text = data.utf8String.replaceAll("[\r\n]+$", "")
      this.handleInput(text)
      log.info("Received {} from remote address {}", text, remote)
    case _: Tcp.ConnectionClosed =>
      log.info("Stopping, because connection for remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.info("Stopping, because connection for remote address {} died", remote)
      context.stop(self)
    case s =>
      log.info("Received " + s)
  }
  context.watch(connection)
  
  def receiveText(str: String, prompt: Boolean = true, newline: Boolean = true) {
    if(newline) {
      this.write(ByteString(str + "\n"))
    } else {
    	this.write(ByteString(str))
    }
    
    if(prompt) {
      if(!newline) this.write(ByteString("\n> "))
      else this.write(ByteString("> "))
    }
  }
  
  def getCommandSource[S <: GameEntity](): S = this.player.asInstanceOf[S]
  
  def handleInput(text: String) {
    if(text.trim() == "") {
      this.receiveText("")
      return
    }

    val res:CommandResult = this.parseCommand(text)
    res match {
      case CommandSuccess(_,_,_) =>

      case CommandFailure(_,_,_) =>
        val name = res.command.name
        this.receiveText(s"Invalid syntax for command: $name")

      case NoCommand() =>
        this.receiveText(s"No such command: $text")
    }
  }

  /**
   * Login stuff.
   */
  BasicUtilityCommands.giveAll(this)
  
  var player:Mob = new Mob()
  player.client = this
  player.name = "Named"
  player.move(TheVoid)
}