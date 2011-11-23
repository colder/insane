package insane

import utils.Reporters.Reporter
import utils.Settings
import utils.Verbosity
import utils.XMLConfig

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin,PluginComponent}

class InsanePlugin(val global: Global) extends Plugin {

  val name = "insane"
  val description = "INterprocedural Shape ANalysis Engine"

  val settings = new Settings()
  var displayUsage  = false

  val reporter = new Reporter(global, settings)

  /** The help message displaying the options for that plugin. */
  override val optionsHelp: Option[String] = Some(
    " Output Control:" + "\n" +
    "  --drawpt=name          Queries the DB and draw corresponding graph" + "\n" +
    "  --dumpcfg=s1:s2        Dumps CFG for the given symbols, _ for all" + "\n" +
    "  --dumppt=s1:s2         Dumps Point-to graphs for the given symbols, _ for all" + "\n" +
    "  --debugfun=s1:s2       Debug given function symbols" + "\n" +
    "  --displayta=s1:s2      Displays Type Analysis results for the given symbols, _ for all" + "\n" +
    "  --displaypure=s1:s2    Displays Purity info for the given symbols, _ for all" + "\n" +
    "  --dumphierarchy        Dumps class hierarchy graph" + "\n" +
    "  --dumpcallgraph        Dumps call graph resulting of class analysis" + "\n" +
    "  --verbosity=normal     Sets verbosity (quiet < normal < verbose < debug)" + "\n" +
    "  --verbose              Sets verbosity to verbose" + "\n" +
    "  --quiet                Sets verbosity to quiet" + "\n" +
    "  --debug                Sets verbosity to debug" + "\n" +
    "\n" +
    " Setting up the environment:" + "\n" +
    "  --config=cfg.xml       Use the provided xml file to configure the access to the database" + "\n" +
    "  --createtables         Initialize the database structure by creating SQL tables" + "\n" +
    "  --fillhierarchy        Fills the database with the class hierarchy computed in this analysis" + "\n" +
    "  --fillgraphs           Fills the database with the graphs computed in this analysis" + "\n" +
    "\n" +
    " Miscellaneous:" + "\n" +
    "  --help                 Displays this help" + "\n"
  )

  var compileTimeStart = 0l

  def init() {
      reporter.msg("""    _                            """)
      reporter.msg("""   (_)___  _________  ____  ___  """)
      reporter.msg("""  / / __ \/ ___/ __ `/ __ \/ _ \ """)
      reporter.msg(""" / / / / (__  ) /_/ / / / /  __/ """)
      reporter.msg("""/_/_/ /_/____/\__,_/_/ /_/\___/  """)
      reporter.msg("")
      reporter.msg("Compiling...")

      compileTimeStart = System.currentTimeMillis
  }

  /** Processes the command-line options. */
  private def splitList(lst: String) : Seq[String] = lst.split(':').map(_.trim).filter(!_.isEmpty)

  override def processOptions(options: List[String], error: String => Unit) {
    var setVerbosity = false

    for(option <- options) {
      option.split("=", 2).toList match {
        case "config"   :: path :: Nil  =>
          new XMLConfig(path).load(settings)

        case "drawpt"   :: s :: Nil  =>
          settings.drawpt = Some(s)

        case "dumpcfg"   :: symbols :: Nil  =>
          settings.dumpcfgs = splitList(symbols)

        case "dumppt"   :: symbols :: Nil  =>
          settings.dumpptgraphs = splitList(symbols)

        case "debugfun"   :: symbols :: Nil  =>
          settings.dumpptgraphs     = splitList(symbols)
          settings.dumpcfgs         = splitList(symbols)
          settings.debugfunctions   = splitList(symbols)

        case "displaypure"   :: symbols :: Nil  =>
          settings.displaypure = splitList(symbols)

        case "dumphierarchy" :: Nil  =>
          settings.dumpClassDescendents = true
        case "dumpcallgraph" :: Nil  =>
          settings.dumpCallGraph = true

        case "verbosity" :: verb :: Nil     =>
          if (setVerbosity) {
            error("Can't set verbosity twice")
          }
          verb.toLowerCase match {
            case "quiet"   => settings.verbosity = Verbosity.Quiet
            case "normal"  => settings.verbosity = Verbosity.Normal
            case "verbose" => settings.verbosity = Verbosity.Verbose
            case "debug"   => settings.verbosity = Verbosity.Debug
            case _         => error("Invalid verbosity: "+verb)
          }
          setVerbosity = true

        case "verbose" :: Nil  =>
          if (setVerbosity) {
            error("Can't set verbosity twice")
          }
          settings.verbosity = Verbosity.Verbose
          setVerbosity = true

        case "createtables" :: Nil  =>
          settings.createTables = true

        case "fillhierarchy" :: Nil  =>
          settings.fillHierarchy= true

        case "fillgraphs" :: Nil  =>
          settings.fillGraphs = true

        case "quiet" :: Nil  =>
          if (setVerbosity) {
            error("Can't set verbosity twice")
          }
          settings.verbosity = Verbosity.Verbose
          setVerbosity = true

        case "help" :: Nil  =>
          displayUsage = true

        case "debug" :: Nil  =>
          if (setVerbosity) {
            error("Can't set verbosity twice")
          }
          settings.verbosity = Verbosity.Debug
          setVerbosity = true

        case "displayta"   :: symbols :: Nil =>
          settings.displaytypeanalyses = splitList(symbols)

        case _ =>
          error("Invalid option: " + option)
      }
    }
  }

  val analysisComponent  = new AnalysisComponent(this, reporter, settings) {
    val global: InsanePlugin.this.global.type = InsanePlugin.this.global
  }

  val components = List[PluginComponent](analysisComponent)
}
