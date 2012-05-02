package insane
package utils

import scala.tools.nsc.Global
import tools.nsc.util._

object Reporters {

  abstract class ReporterFormatter {
    def formatTypeTitle(typ: MsgType): String

    def asTitle(str: String): String
  }

  class ConsoleFormatter extends ReporterFormatter {
    def formatTypeTitle(typ: MsgType) = {
      typ match {
        case FatalMsg =>
          Console.RED+Console.BOLD+typ.title+Console.RESET
        case ErrorMsg =>
          Console.RED+typ.title+Console.RESET
        case WarningMsg =>
          Console.YELLOW+typ.title+Console.RESET
        case NormalMsg =>
          Console.MAGENTA+typ.title+Console.RESET
        case DebugMsg =>
          typ.title
      }
    }

    def asTitle(str: String) = {
      Console.BLUE+Console.BOLD+str+Console.RESET
    }
  }

  class PlainFormatter extends ReporterFormatter {
    def asTitle(str: String) = str

    def formatTypeTitle(typ: MsgType) = {
      typ.title
    }
  }

  sealed abstract class MsgType {
    val title: String
  }

  case object FatalMsg extends MsgType {
    val title = "fatal  "
  }

  case object ErrorMsg extends MsgType {
    val title = "error  "
  }

  case object NormalMsg extends MsgType {
    val title = "info   "
  }

  case object WarningMsg extends MsgType {
    val title = "warning"
  }

  case object DebugMsg extends MsgType {
    val title = "debug  "
  }

  final case class MsgLines(lines: Seq[String]);

  case class Msg(lines: Seq[String], typ: MsgType, indent: Int = 0) {
    def content = lines.mkString("\n")

    val firstLine  = lines.head
    val otherLines = lines.tail
  }

  import language.implicitConversions

  implicit def posToOptPos(p: Position): Option[Position] = Some(p)
  implicit def strToMsgLines(m: String): MsgLines         = MsgLines(Seq(m))
  implicit def seqStrToMsgLines(m: Seq[String]): MsgLines = MsgLines(m)

  class Reporter(global: Global, settings: Settings) {
    val output = new OutputHandlers.Console
    val debug  = new OutputHandlers.Debug

    var messages: List[(Msg, Option[Position])] = Nil

    var currentIndent: Int = 0;
    val indentStep         = 8;

    def incIndent() {
      currentIndent += indentStep
    }
    def decIndent() {
      currentIndent -= indentStep
    }

    def isTerminal = (System.getenv("TERM") != null) && (System.getenv("TERM").length > 0)

    var errorsCounter   = 0
    var warningsCounter = 0

    val formatter = {
      if (isTerminal) {
        new ConsoleFormatter
      } else {
        new PlainFormatter
      }
    }

    def getProgressBar(max: Int, size: Int = 40): ProgressBar = {
      if (settings.immediateReport || settings.displayFullProgress) {
        new HiddenProgressBar(max, size)
      } else {
        if (isTerminal) {
          new ConsoleProgressBar(max, size)
        } else {
          new PlainProgressBar(max, size)
        }
      }
    }

    def fatalError(msg: String) = sys.error(msg)

    private def printText(content: String) {
      output.print(content)
    }

    private def storeMessage(msg: Msg, optPos: Option[Position]) {
      messages = (msg, optPos) :: messages
    }

    private def dispatchMessage(msg: Msg, optPos: Option[Position]) {
      if (settings.immediateReport) {
        msg.typ match {
          case ErrorMsg =>
            errorsCounter += 1

          case WarningMsg =>
            warningsCounter += 1

          case _ =>
        }

        printMessage(msg, optPos)
      } else {
        storeMessage(msg, optPos)
      }
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

      val stats = msgs.groupBy(_._1.typ)

      val nErrors   = stats.getOrElse(ErrorMsg, Seq()).size + errorsCounter
      val nWarnings = stats.getOrElse(WarningMsg, Seq()).size + warningsCounter

      printText(nErrors+" error"+(if(nErrors != 1) "s" else "")+" and "+nWarnings+" warning"+(if(nWarnings != 1) "s" else "")+".\n")

    }

    private def printMessage(msg: Msg, optPos: Option[Position]) {
      val strPos = optPos match {
          case Some(posIn) =>
            val pos = if (posIn eq null) NoPosition
                 else if (posIn.isDefined) posIn.inUltimateSource(posIn.source)
                 else posIn

            pos match {
              case FakePos(fmsg) =>
                "?:? ("+fmsg+"): "
              case NoPosition =>
                ""

              case _ =>
                val file = pos.source.file

                file.path+":"+pos.line+": "
            }

           case None =>
            ""
      }

      val indent = " "*msg.indent
      printText(strPos+formatter.formatTypeTitle(msg.typ)+": "+indent+msg.firstLine+"\n")
      for (line <- msg.otherLines) {
        printText(" "*(strPos+msg.typ.title+": "+indent).length + line+"\n")
      }

      optPos match {
          case Some(posIn) if posIn ne null=>
            val pos = if (posIn.isDefined) posIn.inUltimateSource(posIn.source)
                      else posIn

            pos match {
              case FakePos(fmsg) =>
              case NoPosition =>
              case _ =>
                printSourceLine(pos)
            }
          case _ =>
      }
    }

    private def printSourceLine(pos: Position) = {
      printText(pos.lineContent.stripLineEnd+"\n")
      if (pos.isDefined) {
        printText((" " * (pos.column - 1) + "^\n"))
      }
    }

    def msg(m: MsgLines,   optPos: Option[Position] = None) = printMessage(   Msg(m.lines, NormalMsg,  currentIndent), optPos)
    def info(m: MsgLines,  optPos: Option[Position] = None) = dispatchMessage(Msg(m.lines, NormalMsg,  currentIndent), optPos)
    def error(m: MsgLines, optPos: Option[Position] = None) = dispatchMessage(Msg(m.lines, ErrorMsg,   currentIndent), optPos)
    def fatal(m: MsgLines, optPos: Option[Position] = None) = {
      printMessage(Msg(m.lines, FatalMsg,   currentIndent), optPos)
      sys.error("Panic!")
    }
    def debug(m: MsgLines, optPos: Option[Position] = None) = dispatchMessage(Msg(m.lines, DebugMsg,   currentIndent), optPos)
    def warn(m: MsgLines,  optPos: Option[Position] = None) = dispatchMessage(Msg(m.lines, WarningMsg, currentIndent), optPos)

    def title(m: String) {
      msg(formatter.asTitle(m))
    }

    private def debugDetails() {
      val sw = new java.io.StringWriter
      new Exception().printStackTrace(new java.io.PrintWriter(sw))

      val trace = sw.toString.split("\n").drop(3).foreach(l => printText(l+"\n"))
    }
  }

  case class CompilerReporterPassThrough(as: (String, Position) => Unit) extends scala.tools.nsc.reporters.Reporter {
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
      as(msg, pos)
    }
  }
}
