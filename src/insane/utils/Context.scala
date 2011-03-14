package insane
package utils

import scala.tools.nsc._
import scala.tools.nsc.plugins._

trait Context extends Functions {
  self: AnalysisComponent =>

  import global._
  import global.definitions._

  var funDecls = Map[Symbol, AbsFunction]()
}
