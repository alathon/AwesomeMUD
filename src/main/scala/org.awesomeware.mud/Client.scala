package org.awesomeware.mud

import java.io._
import akka.actor._
import java.net.{InetSocketAddress, Socket}
import akka.util.ByteString
import akka.io.Tcp

object Client {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new Client(remote, connection))
}

class Client(remote:InetSocketAddress, connection:ActorRef) extends Actor with ActorLogging {
  context.watch(connection)

  def receive: Receive = {
    case Tcp.Received(data: ByteString) =>
      val text = data.utf8String.trim
      log.info("Received {} from remote address {}", text, remote)
      sender ! Tcp.Write(data)
    case _: Tcp.ConnectionClosed =>
      log.info("Stopping, because connection for remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.info("Stopping, because connection for remote address {} died", remote)
      context.stop(self)
  }
}
