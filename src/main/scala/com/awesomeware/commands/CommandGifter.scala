package com.awesomeware.commands

/**
 * Utility trait to make giving commands as 'sets' easier.
 */

trait CommandGifter {
  val commands: Set[Command]

  def giveAll(to: Commander) {
    to.addCommands(this.commands.filter(cmd => cmd.isEligable(to)))
  }

  def removeAll(from: Commander) {
    from.commands --= this.commands
  }
}