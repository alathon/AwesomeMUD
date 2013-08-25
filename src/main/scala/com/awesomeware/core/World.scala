package com.awesomeware.core

import scala.collection.mutable
import com.awesomeware.core.io.Client

object World {
  val clients = new mutable.HashSet[Client] with mutable.SynchronizedSet[Client]
  val version = "AwesomeMUD v0.1"
}
