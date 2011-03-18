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

    class ClassAnalysisEnv(dfacts: Map[CFG.Ref, ObjectInfo]) extends DataFlowEnvAbs[ClassAnalysisEnv, CFG.Statement] {
      var isBottom = false

      var facts = dfacts

      def setFact(t : (CFG.Ref, ObjectInfo)) = {
          facts += t
      }
      def getFact(r: CFG.Ref) = facts(r)

      def this() = this(Map[CFG.Ref, ObjectInfo]().withDefaultValue(ObjectSet.empty));

      def copy = new ClassAnalysisEnv(facts)

      def union(that: ClassAnalysisEnv) = {
        var newFacts = Map[CFG.Ref, ObjectInfo]().withDefaultValue(ObjectSet.empty)

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
        (facts.map { case (k, v) => k+" => "+v } mkString("{", "; ", "}"))
      }
    }

    object BaseClassAnalysisEnv extends ClassAnalysisEnv(Map()) {
      override def copy = new ClassAnalysisEnv(Map())
      override def union(that: ClassAnalysisEnv) = that

      override def equals(e: Any) = {
          if (e.isInstanceOf[AnyRef]) {
              BaseClassAnalysisEnv eq e.asInstanceOf[AnyRef]
          } else {
              false
          }
      }

      override def toString = {
        "<base>"
      }

      isBottom = true
    }

    def isAnyVal(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe <:< definitions.AnyValClass.tpe}

    class ClassAnalysisTF extends TransferFunctionAbs[ClassAnalysisEnv, CFG.Statement] {
      type Env = ClassAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.copy

        st match {
          case (av: CFG.AssignVal) => av.v match {
            case r2: CFG.Ref =>
              env setFact (av.r -> env.getFact(r2))
            case _: CFG.LiteralValue =>
              // irrelevant call
          }
          case (aa: CFG.AssignArg) =>
            env setFact(aa.r -> getDescendants(aa.symbol.tpe.typeSymbol))

          case (as: CFG.AssignSelect) =>
            env setFact(as.r -> getDescendants(as.field.tpe.typeSymbol))

          case aam: CFG.AssignApplyMeth =>
            aam.getTree match {
              case a : Apply if isAnyVal(a.symbol.owner) =>
                // If the apply is owned by a class that extends AnyVal we can safely ignore the method call
              case _ =>
                aam.meth.tpe match {
                  case MethodType(args, ret) =>
                    env setFact(aam.r -> getDescendants(ret.typeSymbol))
                  case _ =>
                    reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe)
                }
            }
          case an: CFG.AssignNew =>
            // an.symbol is the constructor symbol
            env setFact (an.r -> ObjectSet.singleton(an.symbol.owner))
          case CFG.Skip | _: CFG.Branch | _: CFG.Assert =>
            // ignored
        }

        env
      }
    }

    def analyze(f: AbsFunction) {
      val cfg       = f.cfg.get
      val bottomEnv = BaseClassAnalysisEnv;
      val baseEnv   = new ClassAnalysisEnv();

      val ttf = new ClassAnalysisTF
      val aa = new DataFlowAnalysis[ClassAnalysisEnv, CFG.Statement](bottomEnv, baseEnv, settings)
      if (settings.displayClassAnalysis(f.symbol.fullName)) {
        reporter.info("Analyzing "+f.symbol.fullName+"...")
      }

      aa.computeFixpoint(cfg, ttf)

      if (settings.displayFixPoint) {
          println("     - Fixpoint:");
          for ((v,e) <- aa.getResult.toSeq.sortWith{(x,y) => x._1.name < y._1.name}) {
              println("      * ["+v+"] => "+e);
          }
      }

      if (settings.displayClassAnalysis(f.symbol.fullName)) {
        def printResults(s: CFG.Statement, e:ClassAnalysisEnv) = s match {
          case aam: CFG.AssignApplyMeth =>
            aam.getTree match {
              case a : Apply if !isAnyVal(a.symbol.owner) =>
                aam.meth.tpe match {
                  case MethodType(args, ret) =>
                    reporter.info("In method call "+a+", "+aam.obj+" is of class "+e.getFact(aam.obj))
                  case _ =>
                    reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe)
                }
              case _ => // ignore
            }
          case _ => // ignore
        }
        aa.pass(cfg, printResults)
      }
    }

    def run {
      // Initialize worklist
      for ((sym, f) <- funDecls) {
        analyze(f)
      }
    }
  }
}
