package insane

import scala.tools.nsc.{Global,Settings,CompilerCommand}
import scala.tools.nsc.reporters.ConsoleReporter

object Main {
  def main(args : Array[String]) {
    run(args)
  }

  def run(args: Array[String], classPath : Option[Seq[String]] = None) {
    val settings = new Settings
    classPath.foreach(s => settings.classpath.tryToSet(s.toList))

    // make sure that the java classpath, containing ., is not used by insane
    System.setProperty("scala.usejavacp", "false")

    runWithSettings(args, settings)
  }

  private def runWithSettings(args : Array[String], settings : Settings) {

    val (insaneOptions, compilerOptions) = args.toList.partition(_.startsWith("--"))

    val command = new CompilerCommand(compilerOptions, settings) {
      override val cmdName = "scalac-insane"
    }

    if(command.ok) {
      if(settings.version.value) {
        println(command.cmdName + " beta.")
      } else {
        val runner = new PluginRunner(settings)
        runner.insanePlugin.processOptions(insaneOptions.map(_.substring(2)), Console.err.println(_))

        val run = new runner.Run
        run.compile(command.files)
      }
    }
  }
}

/** This class is a compiler that will be used for running the plugin in
 * standalone mode. Original version courtesy of D. Zufferey. */
class PluginRunner(settings : Settings) extends Global(settings, new ConsoleReporter(settings)) {

  val insanePlugin = new InsanePlugin(this)

  override protected def computeInternalPhases() {
    val phases = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      superAccessors          -> "add super accessors in traits and nested classes",
      pickler                 -> "serialize symbol tables",
      refchecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns",
      erasure                 -> "erase types, add interfaces for traits",
      lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
      lambdaLift              -> "move nested functions to top level",
      constructors            -> "move field definitions into constructors",
      mixer                   -> "mixin composition"
    ).map(_._1) ::: insanePlugin.components

    for (phase <- phases) {
      phasesSet += phase
    }
  }
}
