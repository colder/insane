package insane
package analysis

import utils._
import utils.Graphs._
import CFG._

trait PurityAnalysis {
  self: AnalysisComponent =>

  import global._


  class PurityAnalysisPhase extends SubPhase {
    val name = "Purity Analysis"

    var simplePureFunctions = Map[Symbol, Boolean]().withDefaultValue(false)
    var reallyPureFunctions = Map[Symbol, Boolean]()

    def run = {

      // Step 1, check whether the functions updates a field
      for (fun <- funDecls.values) {
        simplePureFunctions += fun.symbol -> true
        new ForeachTreeTraverser(traverseStep(fun.symbol)).traverse(fun.body)
      }

      // Step 2, check whether pure functions calls unpure functions
      for (fun <- funDecls.values) {
        isReallyPure(fun.symbol)
      }

      if (!settings.displaypure.isEmpty) {
        val symbols = reallyPureFunctions.keys.filter(s => settings.displayPure(s.fullName))

        reporter.info("Pure Methods: ")
        for(s <- symbols if reallyPureFunctions(s)) {
          reporter.info("  "+s.fullName)
        }
        reporter.info("Non-pure Methods: ")
        for(s <- symbols if !reallyPureFunctions(s)) {
          reporter.info("  "+s.fullName)
        }
      }
    }

    def traverseStep(symbol: Symbol)(tr: Tree) = tr match {
        case Assign(s @ Select(o, field), rhs) =>
          simplePureFunctions += symbol -> false

        case Assign(i @ Ident(name), rhs) =>
          if (i.symbol.owner != symbol) {
            simplePureFunctions += symbol -> false
          }
        case _ =>
    }

    var processing = Set[Symbol]()
    def isReallyPure(symbol: Symbol): Boolean = {
      val r = if (!simplePureFunctions(symbol)) {
        false
      } else if (reallyPureFunctions contains symbol) {
        reallyPureFunctions(symbol)
      } else {
        classAnalysisGraph.mToV.get(symbol) match {
          case Some(vertex) =>
            val toCheck = vertex.out.map(_.v2.symbol) -- processing

            processing += symbol
            val r = toCheck.forall(isReallyPure(_))
            processing -= symbol
            r
          case None =>
            false
        }
      }
      reallyPureFunctions += symbol -> r
      r
    }
  }
}
