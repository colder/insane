package insane
package utils

import scala.tools.nsc.Global

class Reporter(global: Global, settings: Settings) {

  def fatalError(msg: String) = sys.error(msg)

  def out(content: String)       = print(content)
  def outPrompt(content: String) = out("insane: "+content)

  def info(msg: String) = outPrompt(msg+"\n")

  def error(msg: String) = {
    info("["+Console.RED+"error"+Console.RESET+"] "+Console.RED+msg+Console.RESET)
    if (settings.debugMode) {
      debugDetails()
    }
  }

  def warn(msg: String) = {
    info("["+Console.YELLOW+"warn"+Console.RESET+"] "+Console.YELLOW+msg+Console.RESET)
    if (settings.debugMode) {
      debugDetails()
    }
  }

  def title(msg: String) = {
    info(Console.BLUE+Console.BOLD+msg+Console.RESET)
  }

  private def debugDetails() = {
    val sw = new java.io.StringWriter
    new Exception().printStackTrace(new java.io.PrintWriter(sw))

    val trace = sw.toString.split("\n").drop(3).foreach(l => out(l+"\n"))
  }
}
