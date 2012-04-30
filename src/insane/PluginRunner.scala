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

  object guardedLambdaLift extends {
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
            insanePlugin.reporter.error(List(
              "Phase \"lambdaLift\" crashed on "+unit.source.path+":",
              " "+e.getMessage))

            e.printStackTrace
        }
      }

    }
  }

  object guardedConstructors extends {
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
            insanePlugin.reporter.error(List(
              "Phase \"constructors\" crashed on "+unit.source.path+":",
              " "+e.getMessage))

            e.printStackTrace
        }
      }
    }
  }

  override protected def computeInternalPhases() {
    var phasesDesc = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns"
    )

    if (insanePlugin.settings.runErasure) {
      phasesDesc :::= List(
        erasure                 -> "erase types, add interfaces for traits",
        postErasure             -> "clean up erased inline classes",
        lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
        lambdaLift              -> "move nested functions to top level",
        constructors            -> "move field definitions into constructors",
        flatten                 -> "eliminate inner classes"
      )
    } else {
      phasesDesc :::= List(
        earlyLazyVals           -> "allocate bitmaps, translate lazy vals into lazified defs",
        guardedLambdaLift       -> "(guarded) move nested functions to top level",
        guardedConstructors     -> "(guarded) move field definitions into constructors",
        flatten                 -> "eliminate inner classes"
      )
    }

    val phases = phasesDesc ::: insanePlugin.componentsDesc

    phases foreach (addToPhasesSet _).tupled
  }
}
