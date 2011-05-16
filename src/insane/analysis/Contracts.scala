package insane
package analysis

import scala.tools.nsc._
trait Contracts {
  val global: Global

  import global._

  sealed abstract class AbsContract;

  case class Requires(expr: Tree) extends AbsContract;
  case class Ensures(expr: Tree) extends AbsContract;
  case class Assert(tree: Tree, expr: Tree) extends AbsContract;
}
