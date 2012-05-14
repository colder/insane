package insane

import utils.Reporters._
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

  var reporter = new Reporter(global, settings)

  /** The help message displaying the options for that plugin. */
  override val optionsHelp: Option[String] = Some(
    " Output Control:" + "\n" +
//    "  --drawpt=name              Queries the DB and draw corresponding graph" + "\n" +
    "  --ondemand=s1:s2          Only analyze the specified symbols and their dependencies, _ for all" + "\n" +
    "  --dumpcfg=s1:s2           Dumps CFG for the given symbols, _ for all" + "\n" +
    "  --dumppt=s1:s2            Dumps Point-to graphs for the given symbols, _ for all" + "\n" +
//    "  --debugfun=s1:s2          Debug given function symbols" + "\n" +
    "  --displayta=s1:s2         Displays Type Analysis results for the given symbols, _ for all" + "\n" +
    "  --displaypure=s1:s2       Displays Purity info for the given symbols, _ for all" + "\n" +
    "  --dumphierarchy           Dumps class hierarchy graph" + "\n" +
    "  --dumpcallgraph           Dumps call graph resulting of class analysis" + "\n" +
    "  --dumpcallstats           Dumps stats on call targets refinement" + "\n" +
    "  --verbosity=normal        Sets verbosity (quiet < normal < verbose < debug)" + "\n" +
    "  --verbose                 Sets verbosity to verbose" + "\n" +
    "  --quiet                   Sets verbosity to quiet" + "\n" +
    "  --debug                   Sets verbosity to debug" + "\n" +
    "\n" +
    " Analysis Settings:" + "\n" +
    "  --depthresolution=n       Allocation-site uniqueness depth-resolution, defaults to 1" + "\n" +
    "  --openworld               Do not assume closed world" + "\n" +
    "  --considerpure=s1:s2      Mark certain methods as pure for the analysis" + "\n" +
    "  --considerarbitrary=s1:s2 Flag certain methods as unanalyzable, delaying their analysis" + "\n" +
    "  --inlineStrategy=strat    Use a certain strategy for handling method calls" + "\n" +
    "       Possible Strategies:" + "\n" +
    "         - smart            Delay based on heuristic depending on the precision of a method call" + "\n" +
    "         - inline           Always inline calls => \"full\" re-usability and efficiency" + "\n" +
    "         - delay            Always delay the analysis =>  \"full\" precision" + "\n" +
    "\n" +
    " Setting up the environment:" + "\n" +
    "  --config=cfg.xml          Use the provided xml file to configure the access to the database" + "\n" +
 //   "  --createtables             Initialize the database structure by creating SQL tables" + "\n" +
 //   "  --fillhierarchy            Fills the database with the class hierarchy computed in this analysis" + "\n" +
 //   "  --fillgraphs               Fills the database with the graphs computed in this analysis" + "\n" +
 //   "  --mehrasure               Disable erasure (this will fail rapidly, so beware)" + "\n" +
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

      compilerProgressBar.draw();
  }

  object Opt {
    def unapplySeq(str: String) = {
      str.split("=", 2).toList match {
        case name :: value :: Nil =>
          Some(List(name.toLowerCase, value))
        case _ =>
          Some(List(str))
      }
    }
  }
  object SymbolList {
    def unapply(str: String) = {
      Some(str.split(':').map(_.trim).filter(!_.isEmpty))
    }
  }

  override def processOptions(options: List[String], error: String => Unit) {
    var setVerbosity = false

    for(option <- options) option match {
      case Opt("config", path)  =>
        new XMLConfig(path).load(settings)

      case Opt("drawpt", s)  =>
        settings.drawpt = Some(s)

      case Opt("html")  =>
        settings.htmlReporter = true
        reporter = new HTMLReporter(global, settings)

      case Opt("dumpcfg", SymbolList(symbols))  =>
        settings.dumpcfgs = symbols

      case Opt("ondemand", SymbolList(symbols))  =>
        settings.onDemandFunctions = symbols
        settings.onDemandMode      = true

//      case Opt("mehrasure")  =>
//        settings.runErasure = false

      case Opt("dumppt", SymbolList(symbols))  =>
        settings.dumpptgraphs = symbols

      case Opt("debugfun", SymbolList(symbols))  =>
        settings.dumpptgraphs     = symbols
        settings.dumpcfgs         = symbols
        settings.debugfunctions   = symbols

      case Opt("considerpure", SymbolList(symbols))  =>
        settings.funcsConsideredPure = symbols

      case Opt("considerarbitrary", SymbolList(symbols))  =>
        settings.funcsConsideredArbitrary = symbols

      case Opt("displaypure", SymbolList(symbols))  =>
        settings.displaypure = symbols

      case Opt("dumphierarchy")  =>
        settings.dumpClassDescendents = true

      case Opt("dumpcallgraph")  =>
        settings.dumpCallGraph = true

      case Opt("dumpcallstats")  =>
        settings.dumpCallStats = true

      case Opt("inlinestrategy", strategy)  =>
        strategy.toLowerCase match {
          case "smart"         => settings.inlineStrategy = settings.InlineStrategies.Smart
          case "alwaysinline"  => settings.inlineStrategy = settings.InlineStrategies.AlwaysInline
          case "inline"        => settings.inlineStrategy = settings.InlineStrategies.AlwaysInline
          case "delay"         => settings.inlineStrategy = settings.InlineStrategies.AlwaysDelay
          case "alwaysdelay"   => settings.inlineStrategy = settings.InlineStrategies.AlwaysDelay
          case _               => error("Invalid inlineStrategy: "+strategy+ "(e.g. smart, alwaysDelay, alwaysInline)")
        }

      case Opt("verbosity", verb)  =>
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

      case Opt("verbose")  =>
        if (setVerbosity) {
          error("Can't set verbosity twice")
        }
        settings.verbosity = Verbosity.Verbose
        setVerbosity = true

      case Opt("createtables")  =>
        settings.createTables = true

      case Opt("fillhierarchy")  =>
        settings.fillHierarchy= true

      case Opt("fillgraphs")  =>
        settings.fillGraphs = true

      case Opt("quiet")  =>
        if (setVerbosity) {
          error("Can't set verbosity twice")
        }
        settings.verbosity = Verbosity.Verbose
        setVerbosity = true

      case Opt("openworld")  =>
        settings.assumeClosedWorld = false

      case Opt("depthresolution", n)  =>
        settings.depthResolution = n.toInt

      case Opt("help")  =>
        displayUsage = true

      case Opt("debug")  =>
        if (setVerbosity) {
          error("Can't set verbosity twice")
        }
        settings.verbosity = Verbosity.Debug
        setVerbosity = true

      case Opt("displayta", SymbolList(symbols))  =>
        settings.displaytypeanalyses = symbols

      case _ =>
        error("Invalid option: " + option)
    }
  }

  lazy val compilerProgressBar = if (reporter.isTerminal) {
    new utils.ConsoleProgressBar(42);
  } else {
    new utils.PlainProgressBar(42);
  }

  class InsaneRun extends InsanePlugin.this.global.Run {
    override def progress(current: Int, total: Int) = {
        compilerProgressBar.setMax(total)
        compilerProgressBar.setCurrent(current)
        compilerProgressBar.setPostfix("  Phase: "+InsanePlugin.this.global.phase.name.capitalize)
        compilerProgressBar.draw()
    }
  }

  lazy val analysisComponent  = new AnalysisComponent(this, reporter, settings) {
    val global: InsanePlugin.this.global.type = InsanePlugin.this.global
  }

  lazy val componentsDesc = List[(PluginComponent, String)](
    analysisComponent -> "pointer/alias analysis voodoo magic"
  )

  lazy val components = componentsDesc.map(_._1)
}
