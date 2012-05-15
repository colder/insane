package insane
package AST

import utils.Reporters.Reporter

import scala.tools.nsc.Global

trait ASTBindings {
  val global: Global
  val reporter: Reporter

  import global._

  trait ASTBound {
    var tree: Option[Tree] = None

    def setTree(t: Tree): this.type = {
      if (tree != None && tree != Some(t)) {
        reporter.warn("Already set tree on "+this)
      }

      tree = Some(t)
      this
    }

    def setTreeFrom(t: ASTBound): this.type = {
      t.tree match {
        case Some(tr) =>
          setTree(tr)
        case None =>
      }

      this
    }

    def getTree: Tree = tree match {
      case Some(t) =>
        t
      case None =>
        reporter.fatal("Invalid tree attached to "+this)
    }
  }
}
