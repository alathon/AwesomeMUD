package com.awesomeware.core.io

import akka.actor._
import java.net.InetSocketAddress

object Main extends App {
  val sys = ActorSystem("AwesomeMUD")
  val endpoint = new InetSocketAddress("0.0.0.0", 8848)
  sys.actorOf(Server.props(endpoint), "Awesome")
  
  readLine(s"Hit ENTER to continue\n")
  sys.shutdown()
}