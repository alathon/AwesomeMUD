package com.awesomeware.core.io

import akka.actor._
import java.net.InetSocketAddress
import akka.io.{IO, Tcp}

object Server {
  def props(endpoint: InetSocketAddress): Props =
    Props(new Server(endpoint))
}

class Server(endpoint: InetSocketAddress) extends Actor with ActorLogging {
  IO(Tcp)(context.system) ! Tcp.Bind(self, endpoint)

  override def receive: Receive = {
    case Tcp.Connected(remote, _) =>
      log.info("Remote address {} connected", remote)
      sender ! Tcp.Register(context.actorOf(Client.props(remote, sender)))

    case Tcp.CommandFailed(cmd) =>
      println("Command failed: " + cmd)
  }
}