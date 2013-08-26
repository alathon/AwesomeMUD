package com.awesomeware.core.io

class InputGrabber {
  val stack = new scala.collection.mutable.Stack[InputReceiver]

  def active: Boolean = !stack.isEmpty

  def receive(str: String, client: Client) {
    if (active && stack.top.receive(str, client)) {
      stack.pop()
    }
  }

  def addInput(in: InputReceiver) {
    stack.push(in)
  }
}

trait InputReceiver {
  def receive(str: String, client: Client): Boolean
}

trait Story {
  var step = 0


}

abstract class CharCreation extends InputReceiver {
  var step = 0
}

class HitEnter extends InputReceiver {
  def receive(str: String, client: Client): Boolean = {
    true
  }
}
