package awesomeware.commands

/**
 * Utility class to make giving commands as 'sets' easier.
 */

trait CommandGifter {
  val commands:Set[Command]
  
  def isEligable(cmd: Command, commander: Commander): Boolean
  
  def giveAll(to: Commander) {
    for(cmd <- commands) {
    	if(isEligable(cmd, to)) {
    	  to.addCommand(cmd)
    	}
    }
  }
}