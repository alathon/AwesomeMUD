package org.awesomeware.mud

/**

Copyright Awesomeware Org

  */

import akka.actor._
import java.net.InetSocketAddress
import akka.io.{IO, Tcp}

object GameServer {
  def props(endpoint: InetSocketAddress): Props =
    Props(new GameServer(endpoint))
}

class GameServer(endpoint: InetSocketAddress) extends Actor with ActorLogging{
  IO(Tcp)(context.system) ! Tcp.Bind(self, endpoint)

  override def receive: Receive = {
    case Tcp.Connected(remote, _) =>
      log.info("Remote address {} connected", remote)
      sender ! Tcp.Register(context.actorOf(Client.props(remote, sender)))
  }
}
