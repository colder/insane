package insane

import CFG.ASTToCFGTransform
import utils.Reporter
import utils.Context
import utils.{ Settings => InsaneSettings }

import scala.tools.nsc._
import scala.tools.nsc.plugins._

class AnalysisComponent(val global: Global, val pluginInstance: InsanePlugin, val reporter: Reporter, val settings: InsaneSettings)
  extends PluginComponent
  with ASTToCFGTransform
  with CodeExtraction
  with Context
{
  import global._

  override val runsRightAfter: Option[String] = Some("refchecks")
  override val runsAfter: List[String] = List("refchecks")

  val phaseName = pluginInstance.name

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      reporter.info("Running Phases...")

      reporter.info("Extracting function declarations...")
      extractFunDecls(unit)

      reporter.info("Extracting CFGs...")
      extractCFGs(unit)

      reporter.info("Finished")
    }
  }
}
