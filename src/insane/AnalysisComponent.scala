package insane

import CFG.ASTToCFGTransform
import utils.Reporter
import analysis._
import utils._

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent

abstract class AnalysisComponent(pluginInstance: InsanePlugin, val reporter: Reporter, val settings: Settings)
  extends PluginComponent
  with Context
  with ASTToCFGTransform
  with CodeExtraction
  with ClassAnalyses
  with ClassDescendants
{
  val global: Global

  import global._

  override val runsRightAfter: Option[String] = Some("mixin")
  override val runsAfter: List[String] = List("mixin")

  val phaseName = pluginInstance.name

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) = ()

    override def run: Unit = {
      reporter.info("Running Phases...")

      reporter.info("Extracting function declarations...")
      for (unit <- currentRun.units) {
        extractFunDecls(unit)
      }

      reporter.info("Extracting CFGs...")
      for (unit <- currentRun.units) {
        extractCFGs(unit)
      }

      reporter.info("Building class graph")
      generateCDGraph()

      reporter.info("Running ClassAnalysis...")
      runClassAnalysis()

      reporter.info("Finished")

    }
  }

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

}
