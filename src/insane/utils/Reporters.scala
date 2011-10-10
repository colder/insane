package insane
package utils

import scala.tools.nsc.Global
import tools.nsc.util._

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
    var messages: List[(String, Option[Position])] = Nil

    def isTerminal = System.getenv("TERM") != null

    val formatter = {
      if (isTerminal) {
        new ConsoleFormatter
      } else {
        new PlainFormatter
      }
    }

    def getProgressBar(max: Int, size: Int = 40): ProgressBar = {
      if (isTerminal) {
        new ConsoleProgressBar(max, size)
      } else {
        new PlainProgressBar(max, size)
      }
    }


    def fatalError(msg: String) = sys.error(msg)

    def printMessage(content: String) {
      print(content)
    }

    def storeMessage(content: String, optPos: Option[Position]) {
      messages = (content, optPos) :: messages
    }

    def printStoredMessages() {
      val msgs = messages.sortWith {
        case ((_, Some(pos1)), (_, Some(pos2))) =>
          pos1 precedes pos2
        case ((_, Some(pos1)), _) =>
          true
        case _ =>
          false
      }
      for ((m,oPos) <- msgs) {
        printMessage(m, oPos)
      }
    }

    def printMessage(content: String, optPos: Option[Position]) {
      optPos match {
        case Some(posIn) =>
          val pos = if (posIn eq null) NoPosition
               else if (posIn.isDefined) posIn.inUltimateSource(posIn.source)
               else posIn

          pos match {
            case FakePos(fmsg) =>
              printMessage(fmsg+" "+content)
            case NoPosition =>
              printMessage(content)
            case _ =>
              val file = pos.source.file
              printMessage(file.path+":"+pos.line+": "+content)
              printSourceLine(pos)
          }
        case _ =>
          printMessage(content)
      }
    }

    def printSourceLine(pos: Position) = {
      printMessage(pos.lineContent.stripLineEnd+"\n")
      if (pos.isDefined) {
        printMessage((" " * (pos.column - 1) + "^\n"))
      }
    }

    def info(m: String, optPos: Option[Position] = None) {
      printMessage(m+"\n", optPos)
    }

    def msg(m: String, optPos: Option[Position] = None) {
      storeMessage(m+"\n", optPos)
    }

    def error(m: String, optPos: Option[Position] = None) {
      if (settings.extensiveDebug) {
        info(formatter.asError("Error")+": "+m, optPos)
        debugDetails()
      } else {
        msg(formatter.asError("Error")+": "+m, optPos)
      }
    }

    def warn(m: String, optPos: Option[Position] = None) {
      if (settings.extensiveDebug) {
        info(formatter.asWarning("Warning")+": "+m, optPos)
        debugDetails()
      } else {
        msg(formatter.asWarning("Warning")+": "+m, optPos)
      }
    }

    def title(msg: String) {
      info(formatter.asTitle(msg))
    }

    private def debugDetails() {
      val sw = new java.io.StringWriter
      new Exception().printStackTrace(new java.io.PrintWriter(sw))

      val trace = sw.toString.split("\n").drop(3).foreach(l => printMessage(l+"\n"))
    }
  }

  case class CompilerReporterPassThrough(as: (String, Position) => Unit) extends scala.tools.nsc.reporters.Reporter {
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
      as(msg, pos)
    }
  }
}
