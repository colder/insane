package insane

import CFG.ASTToCFGTransform
import utils.Reporter
import analysis._
import utils._

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent

class AnalysisComponent(val global: Global, val pluginInstance: InsanePlugin, val reporter: Reporter, val settings: Settings)
  extends PluginComponent
  with Context
  with ASTToCFGTransform
  with CodeExtraction
  with ClassAnalyses
  with ClassDescendants
{
  import global._

  override val runsRightAfter: Option[String] = Some("flatten")
  override val runsAfter: List[String] = List("flatten")

  val phaseName = pluginInstance.name

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      reporter.info("Running Phases...")

      reporter.info("Extracting function declarations...")
      extractFunDecls(unit)

      reporter.info("Extracting CFGs...")
      extractCFGs(unit)

      reporter.info("Running ClassAnalysis...")
      runClassAnalysis(unit)

      reporter.info("Finished")
    }
  }
}
