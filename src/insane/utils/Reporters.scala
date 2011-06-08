package insane
package utils

import scala.tools.nsc.Global
import tools.nsc.util.Position

object Reporters {
  abstract class ReporterFormatter {
    def asError(str: String): String
    def asWarning(str: String): String
    def asTitle(str: String): String
    def asInfo(str: String): String
  }

  class ConsoleFormatter extends ReporterFormatter {
    def asError(str: String) = {
      Console.RED+str+Console.RESET
    }

    def asWarning(str: String) = {
       Console.YELLOW+str+Console.RESET
    }

    def asTitle(str: String) = {
      Console.BLUE+Console.BOLD+str+Console.RESET
    }

    def asInfo(str: String) = str
  }

  class PlainFormatter extends ReporterFormatter {
    def asError(str: String) = str

    def asWarning(str: String) = str

    def asTitle(str: String) = str

    def asInfo(str: String) = str
  }

  implicit def posToOptPos(p: Position): Option[Position] = Some(p)

  class Reporter(global: Global, settings: Settings) {


    val formatter = {
      if (System.getenv("TERM") == null) {
        new PlainFormatter
      } else {
        new ConsoleFormatter
      }
    }

    def fatalError(msg: String) = sys.error(msg)

    def out(content: String)       {
      print(content)
    }

    val prompt = "insane: "
    val blankPrompt = " "*prompt.length

    def outPrompt(content: String, optPos: Option[Position]) {
      out(prompt+content)

      optPos match {
        case Some(pos) =>
          printSourceLine(pos)
        case _ =>
      }
    }

    def printSourceLine(pos: Position) = {
      out(blankPrompt+pos.lineContent.stripLineEnd)
      if (pos.isDefined) { out(blankPrompt+(" " * (pos.column - 1) + "^\n")) }
    }

    def info(msg: String, optPos: Option[Position] = None) {
      outPrompt(msg+"\n", optPos)
    }

    def error(msg: String, optPos: Option[Position] = None) {
      info("["+formatter.asError("error")+"] "+formatter.asError(msg), optPos)
      if (settings.extensiveDebug) {
        debugDetails()
      }
    }

    def warn(msg: String, optPos: Option[Position] = None) {
      info("["+formatter.asWarning("warning")+"] "+formatter.asWarning(msg), optPos)
      if (settings.extensiveDebug) {
        debugDetails()
      }
    }

    def title(msg: String) {
      info(formatter.asTitle(msg))
    }

    private def debugDetails() {
      val sw = new java.io.StringWriter
      new Exception().printStackTrace(new java.io.PrintWriter(sw))

      val trace = sw.toString.split("\n").drop(3).foreach(l => out(l+"\n"))
    }
  }

  case class CompilerReporterPassThrough(as: (String, Position) => Unit) extends scala.tools.nsc.reporters.Reporter {
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
      as(msg, pos)
    }
  }
}
