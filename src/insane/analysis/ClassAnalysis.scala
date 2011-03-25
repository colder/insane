package insane
package analysis

import utils._
import CFG._

trait ClassAnalyses {
  self: AnalysisComponent =>

  import global._

  def runClassAnalysis() = {
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

    def isStableVal(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.typeSymbol == definitions.StringClass || s.tpe <:< definitions.AnyValClass.tpe}

    def getOSetFromRef(env: ClassAnalysisEnv, r: CFG.Ref): ObjectSet = r match {
      case th: CFG.ThisRef =>
        getDescendants(th.getTree.symbol)
      case su: CFG.SuperRef =>
        getDescendants(su.getTree.symbol)
      case r =>
        env.getFact(r)
    }

    class ClassAnalysisTF extends TransferFunctionAbs[ClassAnalysisEnv, CFG.Statement] {
      type Env = ClassAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.copy

        st match {
          case (av: CFG.AssignVal) => av.v match {
            case r2: CFG.Ref =>
              env setFact (av.r -> getOSetFromRef(env, r2))
            case _: CFG.LiteralValue =>
              // irrelevant call
          }

          case (aa: CFG.AssignTypeCheck) =>
            // ignore

          case (aa: CFG.AssignCast) =>
            val oset = getOSetFromRef(env, aa.rhs)
            val newOset = ObjectSet(oset.symbols.filter(s => s.tpe <:< aa.tpe), oset.isExhaustive)
            env setFact(aa.r -> oset)

          case (aa: CFG.AssignArg) =>
            env setFact(aa.r -> getDescendants(aa.symbol))

          case (as: CFG.AssignSelect) =>
            env setFact(as.r -> getDescendants(as.field))

          case aam: CFG.AssignApplyMeth =>
            aam.getTree match {
              case a : Apply if isStableVal(a.symbol.owner) =>
                // If the apply is owned by a class that extends AnyVal we can safely ignore the method call
              case _ =>
                aam.meth.tpe match {
                  case MethodType(args, ret) =>
                    env setFact(aam.r -> getDescendants(ret.typeSymbol))
                  case PolyType(args, ret) =>
                    env setFact(aam.r -> getDescendants(ret.typeSymbol))
                  case _ =>
                    reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+")")
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


    case class CAVertex(sym: Symbol) extends VertexAbs[EdgeSimple[CAVertex]] {
      val name = sym.toString;
    }

    object CAUnknownTarget extends CAVertex(NoSymbol) {
      override val name = "?"
    }

    object CAGraph extends DirectedGraphImp[CAVertex, EdgeSimple[CAVertex]] {
      var cToG = Map[Symbol, Group]()
      var mToV = Map[Symbol, CAVertex]()

      def addClass(s: Symbol): Group = {
        if (!(cToG contains s)) {
          var gr = new Group(s.toString, RootGroup)
          addGroup(gr)
          cToG += s -> gr
        }
        cToG(s)
      }

      def addMethod(s: Symbol): CAVertex= {
        if (!(mToV contains s)) {
          val v = CAVertex(s)
          this += v
          mToV += s -> v

          inGroup(v, addClass(s.owner))
        }
        mToV(s)
      }

      def addMethodCall(from: Symbol, to: Symbol) {
        val vFrom = addMethod(from)
        val vTo   = addMethod(to)

        this += EdgeSimple[CAVertex](vFrom, vTo)
      }

      def addUnknownTarget(from: Symbol) {
        this += CAUnknownTarget
        this += EdgeSimple[CAVertex](addMethod(from), CAUnknownTarget)
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

      def methodCall(obj: CFG.Ref, oset: ObjectSet,  ms: Symbol) {
        val matches = getMatchingMethods(ms, oset.symbols)

        if (settings.displayClassAnalysis(f.symbol.fullName)) {
          reporter.info("In method call, "+obj+" is of class "+oset)
          reporter.info("Possible targets "+(if (oset.isExhaustive) "are " else "include ") + matches.map(ms =>ms.owner.name+"."+ms.name).mkString(", "))
        }

        if (settings.dumpCG(f.symbol.fullName)) {
          for (m <- matches) {
            CAGraph.addMethodCall(f.symbol, m)
          }

          if (!oset.isExhaustive) {
            CAGraph.addUnknownTarget(f.symbol)
          }
        }
      }
      def generateResults(s: CFG.Statement, env: ClassAnalysisEnv) = s match {
        case aam: CFG.AssignApplyMeth =>
          aam.getTree match {
            case a : Apply if !isStableVal(a.symbol.owner) =>
              aam.obj match {
                case objref: CFG.Ref =>
                  aam.meth.tpe match {
                    case MethodType(args, ret) =>
                      methodCall(objref, getOSetFromRef(env, objref), aam.meth)
                    case PolyType(args, ret) =>
                      methodCall(objref, getOSetFromRef(env, objref), aam.meth)
                    case _ =>
                      reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+")")
                  }
                case _ =>
                  reporter.error("Unexpected non-ref object for non-anyval Apply: "+aam.obj)
              }
            case _ => // ignore
          }
        case an: CFG.AssignNew =>
          methodCall(an.r, ObjectSet.singleton(an.symbol.owner), an.symbol) 
        case _ => // ignore
      }

      aa.pass(cfg, generateResults)
    }

    def getMatchingMethods(methodSymbol: Symbol, classes: Set[Symbol]): Set[Symbol] = {
      assert(methodSymbol.isMethod, "Matching methods of non-method type: "+methodSymbol)

      def getMatchingMethodIn(classSymbol: Symbol): Option[Symbol] = {
        val classes = Seq(classSymbol) ++ classSymbol.ancestors

        var res: Option[Symbol] = None
        for (cl <- classes if res.isEmpty) {
          val found = cl.tpe.decls.lookupAll(methodSymbol.name).find(sym => sym.tpe =:= methodSymbol.tpe)

          if (!found.isEmpty) {
            res = Some(found.get)
          }
        }

        if (res.isEmpty) {
          reporter.warn("Could not find "+methodSymbol+" (type: "+methodSymbol.tpe+") in "+classes.mkString(", "))
        }

        res
      }


      classes map { cs => getMatchingMethodIn(cs) } collect { case Some(cs) => cs }
    }


    def run {
      if (!settings.dumpcg.isEmpty) {
        // generating class blocks, and vertices
        funDecls.values.map(_.symbol).groupBy(_.owner).foreach { case (cl, mss) =>
          CAGraph addClass cl
          
          mss.foreach(m => CAGraph addMethod m)
        }
      }

      for ((sym, f) <- funDecls) {
        analyze(f)
      }

      if (!settings.dumpcg.isEmpty) {
        val path = "callgraph.dot"
        reporter.info("Dumping Call Graph to "+path)
        CAGraph.writeDotToFile(path, "Call Graph Analysis")
      }
    }
  }
}
