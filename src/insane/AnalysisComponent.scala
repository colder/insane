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
  with ClassAnalysis
  with ClassDescendents
{
  val global: Global

  import global._

  override val runsRightAfter: Option[String] = Some("mixin")
  override val runsAfter: List[String] = List("mixin")

  val phaseName = pluginInstance.name

  var subPhases: SubPhases =
    new CodeExtractionPhase andThen
    new CFGGenerationPhase andThen
    new ClassDescendentsPhase andThen
    new ClassAnalysisPhase

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) = ()

    def runSubPhases: Unit = {
      for ((ph, i) <- subPhases.phases.zipWithIndex) {
        reporter.title((i+1)+": "+ph.name)
        ph.run
      }
    }

    override def run: Unit = {
      reporter.info("Running Phases...")
      runSubPhases
      reporter.info("Finished")
    }
  }

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

}
