package insane

import scala.tools.nsc.{Global,Settings,Phase}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.transform.LazyVals
import scala.tools.nsc.transform.Constructors
import scala.tools.nsc.transform.LambdaLift
import scala.tools.nsc.plugins.PluginComponent

/** This class is a compiler that will be used for running the plugin in
 * standalone mode. Original version courtesy of D. Zufferey. */
class PluginRunner(settings : Settings) extends Global(settings, new ConsoleReporter(settings)) {

  val insanePlugin = new InsanePlugin(this)

  object earlyLazyVals extends {
    final val FLAGS_PER_WORD = 32
    val global: PluginRunner.this.type = PluginRunner.this
    val runsAfter = List[String]("explicitouter")
    val runsRightAfter = None
  } with LazyVals

  object safeLambdaLift extends {
    val global: PluginRunner.this.type = PluginRunner.this
    val runsAfter = List("lazyvals")
    val runsRightAfter = None
  } with LambdaLift {
    override protected def newTransformer(unit: CompilationUnit): Transformer =
      new SafeLambdaLifter(unit)

    class SafeLambdaLifter(unit: CompilationUnit) extends LambdaLifter(unit) {
      override def transformUnit(unit: CompilationUnit) {
        try {
          super.transformUnit(unit)
        } catch {
          case e =>
            insanePlugin.reporter.error("Phase \"lambdaLift\" crashed on "+unit.source.path+" ("+e.getMessage+")")
        }
      }

    }
  }

  object safeConstructors extends {
    val global: PluginRunner.this.type = PluginRunner.this
    val runsAfter = List("lambdalift")
    val runsRightAfter = None
  } with Constructors {
    override protected def newTransformer(unit: CompilationUnit): Transformer =
        new SafeConstructorTransformer(unit)

    class SafeConstructorTransformer(unit: CompilationUnit) extends ConstructorTransformer(unit) {
      override def transformUnit(unit: CompilationUnit) {
        try {
          super.transformUnit(unit)
        } catch {
          case e =>
            insanePlugin.reporter.error("Phase \"constructors\" crashed on "+unit.source.path+" ("+e.getMessage+")")
        }
      }
    }
  }

  override protected def computeInternalPhases() {
    val phases = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      superAccessors          -> "add super accessors in traits and nested classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns",
      earlyLazyVals           -> "allocate bitmaps, translate lazy vals into lazified defs",
      safeLambdaLift          -> "move nested functions to top level",
      safeConstructors        -> "move field definitions into constructors"
  //    mixer                   -> "mixin composition"
    ).map(_._1) ::: insanePlugin.components

    for (phase <- phases) {
      phasesSet += phase
    }
  }
}
