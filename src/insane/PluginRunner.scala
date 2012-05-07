package insane

import scala.tools.nsc.{Global,Settings,Phase,SubComponent}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.transform.LazyVals
import scala.tools.nsc.transform.Constructors
import scala.tools.nsc.transform.LambdaLift
import scala.tools.nsc.plugins.PluginComponent

/** This class is a compiler that will be used for running the plugin in
 * standalone mode. Original version courtesy of D. Zufferey. */
class PluginRunner(settings : Settings) extends Global(settings, new ConsoleReporter(settings)) {

  val insanePlugin = new InsanePlugin(this)

  override protected def computeInternalPhases() {
    super.computeInternalPhases()

    insanePlugin.componentsDesc foreach (addToPhasesSet _).tupled
  }

  override protected def computePhaseDescriptors: List[SubComponent] = {
    val allPhases = super.computePhaseDescriptors

    // Remove phases after insane
    var seen = false;
    allPhases.filter { x =>
      if (seen) {
        false
      } else if (x.phaseName == "insane") {
        seen = true
        true
      } else {
        true
      }
    }
  }
}
