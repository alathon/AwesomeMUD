package awesomeware.core.io

import akka.actor._
import java.net.InetSocketAddress
import akka.util.ByteString
import akka.io.Tcp
import awesomeware.core.entities.Mob
import awesomeware.content.staticContent._
import awesomeware.core.entities.GameEntity
import awesomeware.commands._
import awesomeware.commands.impl._
import awesomeware.core.World

object Client {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new Client(remote, connection))
}

class Client(remote: InetSocketAddress, connection: ActorRef)
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
      World.clients -= Client.this
      context.stop(self)
    case Terminated(`connection`) =>
      log.info("Stopping, because connection for remote address {} died", remote)
      World.clients -= Client.this
      context.stop(self)
    case s =>
      log.info("Received " + s)
  }

  context.watch(connection)

  def receiveText(str: String, prompt: Boolean = true, color: Boolean = true, newline: Boolean = true) {
    val b = new StringBuilder()
    b ++= str
    if (newline) b ++= "\r\n"

    if (prompt) {
      if (newline) b ++= "> "
      else b ++= "\r\n> "
    }

    // Prevent any kind of color bleeding.

    if (color)
      this.write(ByteString(Color.colorize(b.toString())))
    else
      this.write(ByteString(Color.colorize(b.toString(), false)))
  }

  def getCommandSource[S <: GameEntity](): S = this.player.asInstanceOf[S]

  def handleInput(text: String) {
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

  /**
   * Login stuff.
   */
  World.clients += Client.this
  BasicUtilityCommands.giveAll(this)

  var player: Mob = new Mob()
  player.client = Some(this)
  player.name = "Named"
  player.move(TheVoid, None)
}