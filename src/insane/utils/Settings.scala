package insane
package utils

import scala.util.matching.Regex

class Settings {
  var configPath                = "config.xml"

  var databaseType              = ""
  var databaseDSN               = ""
  var databaseUsername          = ""
  var databasePassword          = ""

  var runErasure                = true

  var verbosity                 = Verbosity.Normal

  var maxInlinableScore         = 10

  object InlineStrategies extends Enumeration {
    val Smart        = Value("Smart")
    val AlwaysInline = Value("AlwaysInline")
    val AlwaysDelay  = Value("AlwaysDelay")
  }
  type InlineStrategy = InlineStrategies.Value

  var inlineStrategy: InlineStrategy = InlineStrategies.Smart

  def debugMode                 = verbosity == Verbosity.Debug

  def displayFixPoint           = verbosity > Verbosity.Normal
  def displayProgress           = verbosity > Verbosity.Normal

  def immediateReport           = debugMode
  def displayFullProgress       = false

  def getMatcher(s: Seq[String]) : (String => Boolean) = {
    if (s.isEmpty) {
      Matchers.none
    } else {
      val needle = s.mkString("|")

      if (needle == "_") {
        Matchers.all
      } else {
        Matchers.withPattern(needle.replace("_", ".*").r)
      }
    }
  }

  object Matchers {
    def withPattern(regex: Regex)(haystack: String) = {
      !regex.findFirstIn(haystack).isEmpty
    }

    def none(haystack: String) = false

    def all(haystack: String) = true
  }

  var displaytypeanalyses = Seq[String]() 

  lazy val displayTypeAnalysis = getMatcher(displaytypeanalyses)

  var debugfunctions       = Seq[String]() 

  lazy val debugFunction = getMatcher(debugfunctions)

  var funcsConsideredPure  = Seq[String]() 

  lazy val consideredPure = getMatcher(funcsConsideredPure)

  var funcsConsideredArbitrary  = Seq[String]() 

  lazy val consideredArbitrary2 = getMatcher(funcsConsideredArbitrary)

  def consideredArbitrary(s: String) = consideredArbitrary2(s) && ! consideredPure(s)

  var onDemandMode         = false
  var onDemandFunctions    = Seq[String]() 

  lazy val onDemandFunction = getMatcher(onDemandFunctions)

  var dumpcfgs             = Seq[String]() 

  lazy val dumpCFG = getMatcher(dumpcfgs)

  var drawpt: Option[String] = None
  var dumpptgraphs           = Seq[String]()

  lazy val dumpPTGraph = getMatcher(dumpptgraphs)

  var displaypure          = Seq[String]() 

  lazy val displayPure = getMatcher(displaypure)

  var toCheck              = Seq[String]() 

  lazy val shouldCheck= getMatcher(toCheck)

  var dumpCallGraph         = false
  var dumpCallStats         = false
  var dumpClassDescendents  = false

  def displayExceptionsWarn  = false //debugMode

  var assumeClosedWorld     = true

  /**
   * This setting specified the depth of a load node chain util the loadnode
   * resolves to itself, creating an imprecise loop to ensure fixpoint.
   */
  var depthResolution       = 1

  /**
   * When constructing a bootstrapping effect for a CFG, we also follow a
   * precise type signatures up to `contSenDepth` depth.
   */
  var contSenDepth       = 1;

  /**
   * Due to generic types, precise analysis would yield precise enough
   * summaries that could then be used along with type instantiation.
   * For that reason, we did not run precise analysis in a context-sensitive
   * fashion, only when blunt mode was required (i.e. recursive)
   * 
   * This setting specifies if we should use context sensitivity in precise
   * mode.
   */
  var contSenWhenPrecise = true;

  var fillHierarchy         = false
  var fillGraphs            = false
  var fillGraphsIteratively = true
  var createTables          = false

  def ifVerbosity(verb: Verbosity.Value)(body: => Unit) {
    if (verbosity >= verb) body
  }

  def ifVerbose(body: => Unit)    = ifVerbosity(Verbosity.Verbose)(body)
  def ifDebug(body: => Unit)      = ifVerbosity(Verbosity.Debug)(body)


  def isTerminal = (System.getenv("TERM") != null) && (System.getenv("TERM").length > 0)
  
  def getAnalysisProgressBar(max: Int = 42, size: Int = 40): ProgressBar = {
    if (immediateReport || displayFullProgress) {
      new HiddenProgressBar(max, size)
    } else {
      if (isTerminal) {
        new ConsoleProgressBar(max, size)
      } else {
        new PlainProgressBar(max, size)
      }
    }
  }

  def getCompilationProgressBar(max: Int = 42, size: Int = 40): ProgressBar = {
    if (isTerminal) {
      new ConsoleProgressBar(max, size)
    } else {
      new PlainProgressBar(max, size)
    }
  }


  def postProcessOptions() {
    if (debugMode && !toCheck.isEmpty) {
      dumpptgraphs ++= toCheck;
      displaypure  ++= toCheck;
    }
  }
}

object Verbosity extends Enumeration {
  val Quiet      = Value("Quiet",      1)
  val Normal     = Value("Normal",     2)
  val Verbose    = Value("Verbose",    3)
  val Debug      = Value("Debug",      4)

  class VerbVal(name: String, val level: Int) extends Val(nextId, name) with Ordered[Value] {
    def compare(that: VerbVal) = level compare that.level
  }

  def Value(name: String, level: Int) = new VerbVal(name, level)
}
