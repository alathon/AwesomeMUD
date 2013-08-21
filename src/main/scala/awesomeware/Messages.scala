package awesomeware

import java.net.Socket

sealed trait Message

case object ClientLoggedIn extends Message

case object ClientLoggedOut extends Message

case class NewSocketConnection(socket: Socket) extends Message

case object ServerStart extends Message

case object ServerStop extends Message