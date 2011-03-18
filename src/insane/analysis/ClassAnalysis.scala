package insane
package analysis

import utils._
import CFG._

trait ClassAnalyses {
  self: AnalysisComponent =>

  import global._

  def runClassAnalysis(unit: CompilationUnit) = {

    val cl = new ClassAnalysis

    cl.run
  }

  class ClassAnalysis {
    type ObjectInfo = ObjectSet

    class ClassAnalysisEnv(dfacts: Map[Ref, ObjectInfo]) extends DataFlowEnvAbs[ClassAnalysisEnv, CFG.Statement] {
      var isBottom = false

      var facts = dfacts

      def setFact(t : (Ref, ObjectInfo)) = {
          facts += t
      }

      def getFact(r: Ref) = facts(r)

      def this() = this(Map[Ref, ObjectInfo]().withDefaultValue(ObjectSet.empty));

      def copy = new ClassAnalysisEnv(facts)

      def union(that: ClassAnalysisEnv) = {
        var newFacts = Map[Ref, ObjectInfo]().withDefaultValue(ObjectSet.empty)

        for(k <- this.facts.keys ++ that.facts.keys) {
          newFacts += k -> (this.facts(k) ++ that.facts(k))
        }

        new ClassAnalysisEnv(newFacts)
      }

      override def equals(that: Any) = that match {
        case a: ClassAnalysisEnv =>
          (a.facts == facts) && (a.isBottom == isBottom)
        case _ => false
      }

      override def toString = { 
        (dfacts.map { case (k, v) => k+" => "+v } mkString("{", "; ", "}"))
      }
    }

    object BaseClassAnalysisEnv extends ClassAnalysisEnv(Map()) {
      override def copy = this
      override def union(that: ClassAnalysisEnv) = that

      override def equals(e: Any) = {
          if (e.isInstanceOf[AnyRef]) {
              BaseClassAnalysisEnv eq e.asInstanceOf[AnyRef]
          } else {
              false
          }
      }

      isBottom = true
    }

    class ClassAnalysisTF extends TransferFunctionAbs[ClassAnalysisEnv, CFG.Statement] {
      type Env = ClassAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.copy

        st match {
          case (av: CFG.AssignVal) => av.v match {
            case r2: CFG.Ref =>
              env setFact (Ref(av.r) -> env.getFact(Ref(r2)))
            case _: CFG.LiteralValue =>
              // irrelevant call
          }
          case (as: CFG.AssignSelect) =>
            env setFact(Ref(as.r) -> allSubTypesOf(as.field))
          case aam: CFG.AssignApplyMeth =>
            aam.getTree match {
              case a : Apply if (a.symbol.owner.tpe <:< definitions.AnyValClass.tpe) =>
                // If the apply is owned by a class that extends AnyVal we can safely ignore the method call
              case _ =>
                aam.meth.tpe match {
                  case MethodType(args, ret) =>
                    env setFact(Ref(aam.r) -> allSubTypesOf(ret.typeSymbol))
                  case _ =>
                    reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe)
                }
            }
          case an: CFG.AssignNew =>
            // an.symbol is the constructor symbol
            env setFact (Ref(an.r) -> ObjectSet.singleton(an.symbol.owner))
          case CFG.Skip | _: CFG.Branch | _: CFG.Assert =>
            // ignored
        }

        env
      }

      def allSubTypesOf(s: Symbol): ObjectSet = getDescendants(s)

      def todo(st: CFG.Tree) {
        reporter.info("Unhandled in TF: "+st)
      }

      def getRef(r: CFG.Ref): Ref = Ref(r)
    }

    case class Ref(val ref: CFG.Ref) {
      override def equals(that: Any) = that match {
        case that: Ref =>
          (ref, that.ref) match {
            case (s1: CFG.SymRef, s2: CFG.SymRef) => s1.symbol == s2.symbol
            case (t1: CFG.TempRef, t2: CFG.TempRef) => t1.name == t2.name
            case _ => false
          }
        case _ => false
      }

      override def toString = ref.toString
    }

    def analyze(cfg: ControlFlowGraph[CFG.Statement]) {
      val bottomEnv = BaseClassAnalysisEnv;
      val baseEnv   = new ClassAnalysisEnv();

      val ttf = new ClassAnalysisTF
      val aa = new DataFlowAnalysis[ClassAnalysisEnv, CFG.Statement](bottomEnv, baseEnv, settings)

      aa.computeFixpoint(cfg, ttf)

      if (settings.displayFixPoint) {
          println("     - Fixpoint:");
          for ((v,e) <- aa.getResult.toSeq.sortWith{(x,y) => x._1.name < y._1.name}) {
              println("      * ["+v+"] => "+e);
          }
      }
    }

    def run {
      // Initialize worklist
      for ((sym, f) <- funDecls) {
        analyze(f.cfg.get)
      }
    }
  }
}
