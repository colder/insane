package insane

import utils.Reporter

import scala.tools.nsc
import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.{Plugin,PluginComponent}

class InsanePlugin(val global: Global) extends Plugin {
  import global._

  val name = "scala-insane"
  val description = "Interprocedural Shape Analysis Engine"

  /** The help message displaying the options for that plugin. */
  override val optionsHelp: Option[String] = None

  /** Processes the command-line options. */
  override def processOptions(options: List[String], error: String => Unit) {
    for(option <- options) {
      option match {
        case _ => error("Invalid option: " + option)
      }
    }
  }

  val reporter = new Reporter(global)
  val components = List[PluginComponent](new AnalysisComponent(global, this, reporter))
}
