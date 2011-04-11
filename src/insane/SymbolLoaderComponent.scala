package insane

import CFG.CFGGeneration
import utils.Reporter
import analysis._
import utils._

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent

abstract class SymbolLoaderComponent(pluginInstance: InsanePlugin, val reporter: Reporter, val settings: Settings)
  extends PluginComponent
{
  val global: Global

  import global._

  override val runsRightAfter: Option[String] = Some("pickler")
  override val runsAfter: List[String] = List("pickler")

  val phaseName = pluginInstance.name+"-loader"

  class SymbolLoaderPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) = ()

    def loadSymbols() {
      var seen = Set[Symbol]()

      def recurseSym(sym: Symbol): Unit = {
        val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol

        if (seen contains tpesym) {
          return
        } else {
          seen += tpesym
        }

        if (tpesym.name == nme.NOSYMBOL) {
          return
        }

        if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage) {
          tpesym.tpe.members.foreach{ sym =>
            recurseSym(tpesym.tpe.memberInfo(sym).typeSymbol)
          }
        } else if (!sym.isMethod && !sym.isValue) {
          reporter.warn("Ingored "+sym)
        }
      }

      recurseSym(definitions.RootClass)
    }

    override def run: Unit = {
      reporter.title("Loading symbols...")
      loadSymbols()
      reporter.info("Continuing normal compilation...")
    }
  }

  def newPhase(prev: Phase) = new SymbolLoaderPhase(prev)

}
