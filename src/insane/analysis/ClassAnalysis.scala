package insane
package analysis

import utils._
import CFG._

trait ClassAnalyses {
  self: AnalysisComponent =>

  import global._

  def runClassAnalysis(unit: CompilationUnit) = {
    val cl = new ClassAnalysis

    for(fun <- funDecls.values) fun.cfg match {
      case Some(cfg) =>
        reporter.info("Analyzing "+fun)
        cl.analyze(cfg)

      case None =>
    }

  }

  class ClassAnalysis {
    class ClassAnalysisEnv(dfacts: Map[CFG.Ref, Set[ObjectId]], dstore: Map[ObjectId, ObjectValue]) extends DataFlowEnvAbs[ClassAnalysisEnv, CFG.Statement] {
      var facts = dfacts
      var store = dstore

      def setFact(t : (CFG.Ref, Set[ObjectId])) = {
          facts += t
      }

      def this() = this(Map[CFG.Ref, Set[ObjectId]]().withDefaultValue(Set[ObjectId]()), Map[ObjectId, ObjectValue]());

      def copy = new ClassAnalysisEnv(dfacts, dstore)

      def union(that: ClassAnalysisEnv) = {
        var newFacts = Map[CFG.Ref, Set[ObjectId]]().withDefaultValue(Set[ObjectId]())
        var newStore = Map[ObjectId, ObjectValue]()

        for(k <- this.facts.keys ++ that.facts.keys) {
          newFacts += k -> (this.facts(k) ++ that.facts(k))
        }

        new ClassAnalysisEnv(newFacts, newStore)
      }

      def registerObject(id: ObjectId, cl: global.Symbol, args: Seq[CFG.SimpleValue]) {
        store += id -> new ObjectValue(cl)
      }
    }

    object BaseClassAnalysisEnv extends ClassAnalysisEnv(Map(), Map()) {
      override def copy = this
      override def union(that: ClassAnalysisEnv) = that

      override def equals(e: Any) = {
          if (e.isInstanceOf[AnyRef]) {
              BaseClassAnalysisEnv eq e.asInstanceOf[AnyRef]
          } else {
              false
          }
      }
    }

    class ClassAnalysisTF extends TransferFunctionAbs[ClassAnalysisEnv, CFG.Statement] {
      type Env = ClassAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.copy

        st match {
          case (av: CFG.AssignVal) => av.v match {
            case r2: CFG.Ref =>
              env setFact (av.r -> env.facts(r2))
            case af: CFG.AnnonFun =>
              todo(af)
            case _: CFG.LiteralValue =>
              // irrelevant call
          }
          case (as: CFG.AssignSelect) =>
            val newFact = (env.facts(as.obj) map (env.store(_).lookupField(as.field.name))).foldRight(Set[ObjectId]())(_ ++ _)
            env setFact (as.r -> newFact)

          case aap: CFG.AssignApplyFun =>
            todo(st)
          case aam: CFG.AssignApplyMeth =>
            todo(st)
          case an: CFG.AssignNew =>
            val id  = ObjectId(an.getTree.id)
            env.registerObject(id, an.cl, an.args)
            env setFact (an.r -> Set(id))
          case CFG.Skip | _: CFG.Branch | _: CFG.Assert =>
            // ignored
        }

        env
      }

      def todo(st: CFG.Tree) {
        reporter.info("Unhandled in TF: "+st)
      }
    }

    class ObjectValue(val cl: global.Symbol) {
      def lookupField(f: global.Name): Set[ObjectId] = lookupField(f.toString)
      def lookupField(f: String): Set[ObjectId] = Set()
    }

    case class ObjectId(id: Long);


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
  }
}
