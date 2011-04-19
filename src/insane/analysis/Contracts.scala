package insane
package analysis

import scala.tools.nsc._
trait Contracts {
  val global: Global

  import global._

  sealed abstract class AbsContract;

  class Requires(val expr: Tree) extends AbsContract;
  class Ensures(val expr: Tree) extends AbsContract;
}
