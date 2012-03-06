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
import scala.tools.util.SignalManager
import scala.util.control.Exception.ignoring

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
  with UniqueIDs
{
  val global: Global

  import global._

  override val runsRightAfter: Option[String] = Some("constructors")
  override val runsAfter: List[String]        = List("constructors")

  val phaseName = pluginInstance.name+"-analysis"

  var subPhases: SubPhases =
    new CodeExtractionPhase   andThen
    new CFGGenerationPhase    andThen
    new ClassHierarchyPhase   andThen
    new TypeAnalysisPhase     andThen
    new PointToAnalysisPhase
    //new PurityAnalysisPhase

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {

    override def erasedTypes: Boolean = false

    def apply(unit: CompilationUnit) { /* nothing */ }

    def runSubPhases() {
      for ((ph, i) <- subPhases.phases.zipWithIndex) {
        reporter.title((i+1)+": "+ph.name)
        ph.run
      }
    }

    def onExit() = {
      reporter.msg("Bailing out...")
      reporter.printStoredMessages()
      sys.exit(1)
    }

    def onForcedExit() = {
      reporter.msg("Aarrgh!")
      sys.exit(1)
    }

    override def run() {
      ignoring(classOf[Exception]) {
        SignalManager("INT") = onExit()
      }

      pluginInstance.compilerProgressBar.setPostfix("Done.")
      pluginInstance.compilerProgressBar.end();

      reporter.msg("Finished ("+(System.currentTimeMillis-pluginInstance.compileTimeStart)+"ms)")
      reporter.msg("Initializing datastructures...")
      val tStart = System.currentTimeMillis

      initializeStorage

      reporter.msg("Starting analysis...")
      runSubPhases()
      reporter.msg("Finished ("+(System.currentTimeMillis-tStart)+"ms)")

      ignoring(classOf[Exception]) {
        SignalManager("INT") = onForcedExit()
      }

      reporter.printStoredMessages
    }
  }

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

}
