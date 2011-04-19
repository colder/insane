package insane
package AST

import utils.Reporter

import scala.tools.nsc.Global

trait ASTBindings {
  val global: Global
  val reporter: Reporter

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
        reporter.fatalError("Invalid tree attached to "+this)
    }
  }
}
