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
    def apply(unit: CompilationUnit): Unit = {
      reporter.info("Running Phases...")

      reporter.info("Extracting function declarations...")
      extractFunDecls(unit)

      reporter.info("Extracting CFGs...")
      extractCFGs(unit)

      reporter.info("Building class graph")
      generateCDGraph()

      reporter.info("Running ClassAnalysis...")
      runClassAnalysis(unit)

      reporter.info("Finished")
    }
  }

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

}
