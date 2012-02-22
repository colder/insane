package insane

import scala.tools.nsc.{Global,Settings,CompilerCommand}

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

        if (runner.insanePlugin.displayUsage) {
          println(command.shortUsage)
          println
          println("where standard options include:")
          println(runner.insanePlugin.optionsHelp.getOrElse(""))
        } else {
          val run = new runner.insanePlugin.InsaneRun
          runner.insanePlugin.init()
          run.compile(command.files)
        }
      }
    }
  }
}

