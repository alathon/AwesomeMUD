package awesomeware.core.io

import akka.actor._
import java.net.InetSocketAddress

object Game extends App {
  val sys = ActorSystem("AwesomeMUD")
  val endpoint = new InetSocketAddress("localhost", 8888)
  sys.actorOf(GameServer.props(endpoint), "Awesome")

  readLine(s"Hit ENTER to continue")
  sys.shutdown()
}