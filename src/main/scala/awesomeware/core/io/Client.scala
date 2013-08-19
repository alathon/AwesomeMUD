package awesomeware.core.io

import akka.actor._
import java.net.{InetSocketAddress, Socket}
import akka.util.ByteString
import akka.io.Tcp
import awesomeware.core.entities.Mob
import awesomeware.commands.impl.WhoCommand
import awesomeware.commands.CommandInterpreter
import scala.collection.mutable.ArrayBuffer
import awesomeware.commands.Command
import awesomeware.content.staticContent._
import awesomeware.commands.Commander
import awesomeware.core.entities.GameEntity
import awesomeware.commands.impl.BasicUtilityCommands

object Client {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new Client(remote, connection))
}

class Client(remote:InetSocketAddress, connection:ActorRef) 
	extends Actor with ActorLogging with Commander {
  /**
   * Input and output
   */
  def receiveText(str: String) {
    this.write(ByteString(str))
  }

  def write(s: ByteString) {
    connection ! Tcp.Write(s)
  }

  def receive: Receive = {
    case Tcp.Received(data: ByteString) =>
      val text = data.utf8String.replaceAll("[\r\n]+$", "")
      this.handleCommand(text)
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

  // Initialization
  context.watch(connection)
  
  /** Commander */
  def getCommandSource[S <: GameEntity](): S = this.player.asInstanceOf[S]
  
  def handleNoCommand(text: String) {
    this.receiveText(s"Command not understood: $text")
  }
  
  def handleInvalidSyntax(text: String, command: Command) {
    val niceName = command.name
    this.receiveText(s"Invalid syntax for command $niceName")
  }
  
  /**
   * Login stuff.
   */
  BasicUtilityCommands.giveAll(this)
  
  var player:Mob = new Mob()
  player.client = this
  player.move(TheVoid)
  this.write(ByteString("\n"))
}