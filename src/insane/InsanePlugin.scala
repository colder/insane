package insane

import utils.Reporter
import utils.Settings
import utils.Verbosity

import scala.tools.nsc
import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.{Plugin,PluginComponent}

class InsanePlugin(val global: Global) extends Plugin {
  import global._

  val name = "insane"
  val description = "INterprocedural Shape ANalysis Engine"

  val settings = new Settings()

  /** The help message displaying the options for that plugin. */
  override val optionsHelp: Option[String] = Some(
    "  -P:insane:dumpcfg=s1:s2        Dumps CFG for the given symbols, _ for all" + "\n" +
    "  -P:insane:dumpclassgraph       Dumps class hierarchy graph" + "\n" +
    "  -P:insane:dumpca=s1:s2         Dumps Class Analysis results for the given symbols, _ for all" + "\n" +
    "  -P:insane:verbosity=normal     Sets verbosity (quiet < normal < verbose)" + "\n" +
    "  -P:insane:displayca=s1:s2      Displays Class Analysis results for the given symbols, _ for all"
  )

  /** Processes the command-line options. */
  private def splitList(lst: String) : Seq[String] = lst.split(':').map(_.trim).filter(!_.isEmpty)

  override def processOptions(options: List[String], error: String => Unit) {
    for(option <- options) {
      option.split("=", 2).toList match {
        case "dumpcfg"   :: symbols :: Nil  =>
          settings.dumpcfg = splitList(symbols)

        case "dumpca"    :: symbols :: Nil  =>
          settings.dumpca = splitList(symbols)

        case "dumpclassgraph" :: Nil  =>
          settings.dumpClassDescendents = true

        case "verbosity" :: verb :: Nil     =>
          verb.toLowerCase match {
            case "quiet"   => settings.verbosity = Verbosity.Quiet
            case "normal"  => settings.verbosity = Verbosity.Normal
            case "verbose" => settings.verbosity = Verbosity.Verbose
            case _         => error("Invalid verbosity: "+verb)
          }

        case "displayca"   :: symbols :: Nil   =>
          settings.displayclassanalyses = splitList(symbols)

        case _                              => error("Invalid option: " + option)
      }
    }
  }

  val reporter = new Reporter(global)

  val analysisComponent  = new AnalysisComponent(this, reporter, settings) {
    val global: InsanePlugin.this.global.type = InsanePlugin.this.global
  }

  val components = List[PluginComponent](analysisComponent)
}
