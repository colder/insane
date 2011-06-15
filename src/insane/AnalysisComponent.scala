package insane

import CFG.CFGGeneration
import alias._
import AST.CodeExtraction
import utils._
import types._
import hierarchy._
import utils.Reporters._
import storage._

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent

abstract class AnalysisComponent(pluginInstance: InsanePlugin, val reporter: Reporter, val settings: Settings)
  extends PluginComponent
  with Context
  with Functions
  with CFGGeneration
  with CodeExtraction
  with TypeAnalysis
  with ClassHierarchy
  with PointToAnalysis
  with ObjectSets
  with TypeHelpers
  with SerializationHelpers
  with Storage
{
  val global: Global

  import global._

  override val runsRightAfter: Option[String] = Some("mixin")
  override val runsAfter: List[String]        = List("mixin")

  val phaseName = pluginInstance.name+"-analysis"

  var subPhases: SubPhases =
    new CodeExtractionPhase   andThen
    new CFGGenerationPhase    andThen
    new ClassHierarchyPhase   andThen
    new TypeAnalysisPhase     andThen
    new PointToAnalysisPhase
    //new PurityAnalysisPhase

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) { /* nothing */ }

    def runSubPhases() {
      for ((ph, i) <- subPhases.phases.zipWithIndex) {
        reporter.title((i+1)+": "+ph.name)
        ph.run
      }
    }

    override def run() {
      val tStart = System.currentTimeMillis

      reporter.info("Welcome to insane!")
      reporter.info("Initializing...")

      initializeStorage

      reporter.info("Starting analysis...")
      runSubPhases()
      reporter.info("Finished ("+(System.currentTimeMillis-tStart)+"ms)")

    }
  }

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

}
