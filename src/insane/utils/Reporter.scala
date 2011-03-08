package insane
package utils

import scala.tools.nsc.Global

class Reporter(global: Global) {
  def fatalError(msg: String) = Predef.error(msg)
}
