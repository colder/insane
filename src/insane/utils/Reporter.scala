package insane
package utils

import scala.tools.nsc.Global

class Reporter(global: Global) {
  def fatalError(msg: String) = Predef.error(msg)

  def info(msg: String) = println("[insane] "+msg)

  def error(msg: String) = info("Error: "+msg)
  def warn(msg: String) = info("Warning: "+msg)
}
