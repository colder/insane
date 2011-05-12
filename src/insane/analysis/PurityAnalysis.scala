package insane
package analysis

import utils._

trait PurityAnalysis {
  self: AnalysisComponent =>

  import global._

  sealed abstract class Modifier
  case class GeneralModifier(symbol: Symbol) extends Modifier
  case class ChainedModifier(chain: Seq[Symbol]) extends Modifier

  sealed abstract class Locality
  case object Fresh extends Locality

  sealed abstract class Call
  case object AnyCall extends Call
  case class ParameterCall(symbol: Symbol, method: Symbol) extends Call


  case class MethodEffects(symbol: Symbol, modifies: Set[Modifier], locality: Set[Locality], calls: Set[Call])

  case class PurityInfo(symbol: Symbol, calledMethods: Set[Symbol], updatesFields: Boolean, isTransPure: Boolean)


  class PurityAnalysisPhase extends SubPhase {
    val name = "Purity Analysis"

    var simplePureFunctions = Map[Symbol, Boolean]().withDefaultValue(false)

    def run {

      // Step 1, check whether the functions updates a field
      simplePureFunctions += NoSymbol -> false

      for (fun <- funDecls.values) {
        simplePureFunctions += fun.symbol -> true
        new ForeachTreeTraverser(traverseStep(fun.symbol)).traverse(fun.body)
      }

      // Step 2, check whether pure functions calls unpure functions
      for (fun <- funDecls.values) {
        val transPure = isTransPure(fun.symbol)

        val outCalls = getOutCalls(fun.symbol)

        purityResults += fun.symbol -> PurityInfo(fun.symbol, outCalls, !simplePureFunctions(fun.symbol), transPure)
      }

      if (!settings.displaypure.isEmpty) {
        val toDisplay = purityResults.collect{ case (s, info) if settings.displayPure(s.fullName) => info }.groupBy(_.isTransPure)

        val stats = toDisplay.map{ case (isPure, infos) => (isPure, infos.size)}.withDefaultValue(0)
        reporter.info("Purity Statistics: "+stats(true)+" pure, "+stats(false)+" non-pure")

        settings.ifVerbose {
          toDisplay.foreach{ case (isPure, infos) =>
            reporter.info((if (isPure) "Pure" else "Non-pure")+ " methods")
            for (info <- infos) {
              reporter.info("  "+info.symbol.fullName+(if (info.updatesFields) " (fields)" else if (!info.isTransPure) " (calls)" else ""))
            }
          }
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
    var transPureCache  = Map[Symbol, Boolean]()

    def getOutCalls(symbol: Symbol): Set[Symbol] = {
      callGraph.mToV.get(symbol) match {
        case Some(vertex) =>
          // If wholeCodeAnalysis, we ignore CAUnknownTarget
          vertex.out.collect{ case e if settings.wholeCodeAnalysis || e.v2 != CAUnknownTarget => e.v2.symbol}
        case None =>
          reporter.warn("Trying to get outCalls of an unknown symbol")
          Set()
      }
    }

    def isTransPure(symbol: Symbol): Boolean = {
      val r = if (!simplePureFunctions(symbol)) {
          false
        } else if (transPureCache contains symbol) {
          transPureCache(symbol)
        } else if (callGraph.mToV contains symbol) {
          val toCheck = getOutCalls(symbol) -- processing

          processing += symbol
          val r = toCheck.forall(isTransPure(_))
          processing -= symbol
          r
        } else {
          false
        }

      transPureCache += symbol -> r
      r
    }
  }
}
