package insane
package utils

import scala.tools.nsc.Global
import scala.tools.nsc.symtab._

trait ASTBindings {
  val global: Global

  import global._

  trait ASTBound {
    var tree: Option[Tree] = None

    def setTree(t: Tree): this.type = {
      if (tree != None) {
        println("Already set tree on "+this)
      }

      tree = Some(t)
      this
    }

    def getTree: Tree = tree match {
      case Some(t) =>
        t
      case None =>
        fatalError("Invalid tree attached to "+this)
    }

  }

}
