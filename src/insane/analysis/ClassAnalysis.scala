package insane
package analysis

import utils._
import utils.Graphs._
import CFG._

trait ClassAnalysis {
  self: AnalysisComponent =>

  import global._

  case class CAVertex(val symbol: Symbol) extends VertexAbs[EdgeSimple[CAVertex]] {
    val name = symbol.toString;
  }

  object CAUnknownTarget extends CAVertex(NoSymbol) {
    override val name = "?"
  }

  class ClassAnalysisGraph extends DirectedGraphImp[CAVertex, EdgeSimple[CAVertex]] {
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

  class ClassAnalysisPhase extends SubPhase {
    type ObjectInfo = ObjectSet

    class ClassAnalysisEnv(dfacts: Map[CFG.Ref, ObjectInfo]) extends DataFlowEnvAbs[ClassAnalysisEnv, CFG.Statement] {
      var isBottom = false

      var facts = dfacts

      def setFact(t : (CFG.Ref, ObjectInfo)) = {
          facts += t
      }
      def getFact(r: CFG.Ref) = facts.get(r) match {
        case Some(f) => f
        case None =>
          reporter.warn("Reference "+r+" not registered in facts")
          new ObjectSet(Set(), false)
      }

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
        getDescendents(th.getTree.symbol)
      case su: CFG.SuperRef =>
        getDescendents(su.getTree.symbol)
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
            case n: CFG.Null =>
              env setFact (av.r -> ObjectSet.empty)

            case _: CFG.LiteralValue =>
              // irrelevant call
          }

          case (aa: CFG.AssignTypeCheck) =>
            // ignore, returns boolean

          case (aa: CFG.AssignArray) =>
            val newOset = ObjectSet(Set(typeRef(NoPrefix, definitions.ArrayClass, List(aa.tpe)).typeSymbol), false)
            env setFact (aa.r -> newOset)

          case (aa: CFG.AssignCast) =>
            val oset = getOSetFromRef(env, aa.rhs)
            var newOset = ObjectSet(oset.symbols.filter(s => s.tpe <:< aa.tpe), oset.isExhaustive)

            if (newOset.symbols.isEmpty) {
              if (oset != AllObjects) {
                settings.ifDebug {
                  reporter.warn("Invalid cast: "+aa.rhs+".$asInstanceOf["+aa.tpe+"] type is "+oset+" at "+aa.getTree.pos)
                }
              }
              newOset = ObjectSet(Set(aa.tpe.typeSymbol), oset.isExhaustive)
            }

            env setFact(aa.r -> newOset)

          case (aa: CFG.AssignArg) =>
            env setFact(aa.r -> getDescendents(aa.symbol))

          case (as: CFG.AssignSelect) =>
            env setFact(as.r -> getDescendents(as.field))

          case aam: CFG.AssignApplyMeth =>
            aam.getTree match {
              case a : Apply if isStableVal(a.symbol.owner) =>
                // If the apply is owned by a class that extends AnyVal we can safely ignore the method call
              case t =>
                aam.meth.tpe match {
                  case MethodType(args, ret) =>
                    env setFact(aam.r -> getDescendents(ret.typeSymbol))
                  case PolyType(args, ret) =>
                    env setFact(aam.r -> getDescendents(ret.typeSymbol))
                  case _ =>
                    reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+") at "+t.pos)
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

      if (settings.displayClassAnalysis(f.symbol.fullName)) {
        if (settings.displayFixPoint) {
            println("     - Fixpoint:");
            for ((v,e) <- aa.getResult.toSeq.sortWith{(x,y) => x._1.name < y._1.name}) {
                println("      * ["+v+"] => "+e);
            }
        }
      }

      def methodCall(call: Tree, obj: CFG.Ref, oset_init: ObjectSet,  ms: Symbol) {

        val oset = if (!oset_init.symbols.isEmpty) {
          oset_init
        } else {
          obj match {
            case CFG.SymRef(symbol) if symbol.isModule =>
              // Object.method, we backpatch oset
              val tpesym = symbol.tpe.typeSymbol

              new ObjectSet(Set(tpesym), tpesym.isFinal)
            case _ =>
              settings.ifVerbose {
                reporter.warn("Empty object pool for "+obj+" with call to "+ms.name+" at "+call.pos)
              }
              oset_init
          }
        }

        val matches = getMatchingMethods(ms, oset.symbols)

        if (settings.displayClassAnalysis(f.symbol.fullName)) {
          reporter.info("Possible targets: "+matches.size +" "+(if (oset.isExhaustive) "bounded" else "unbounded")+" method: "+ms.name)
        }

        for (m <- matches) {
          classAnalysisGraph.addMethodCall(f.symbol, m)
        }

        if (!oset.isExhaustive) {
          classAnalysisGraph.addUnknownTarget(f.symbol)
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
                      methodCall(a, objref, getOSetFromRef(env, objref), aam.meth)
                    case PolyType(args, ret) =>
                      methodCall(a, objref, getOSetFromRef(env, objref), aam.meth)
                    case _ =>
                      reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+")")
                  }
                case _ =>
                  reporter.error("Unexpected non-ref object for non-anyval Apply: "+aam.obj)
              }
            case _ => // ignore
          }
        case an: CFG.AssignNew =>
          methodCall(an.getTree, an.r, ObjectSet.singleton(an.symbol.owner), an.symbol) 
        case _ => // ignore
      }

      aa.pass(cfg, generateResults)
    }

    def getMatchingMethods(methodSymbol: Symbol, classes: Set[Symbol]): Set[Symbol] = {
      assert(methodSymbol.isMethod, "Matching methods of non-method type: "+methodSymbol)

      var failures = Set[Symbol]();

      def getMatchingMethodIn(classSymbol: Symbol): Option[Symbol] = {
        val classes = Seq(classSymbol) ++ classSymbol.ancestors

        var res: Option[Symbol] = None

        for (cl <- classes if res.isEmpty) {
          val found = cl.tpe.decls.lookupAll(methodSymbol.name).find(sym => cl.tpe.memberType(sym) <:< methodSymbol.tpe)

          if (!found.isEmpty) {
            res = Some(found.get)
          }
        }

        if (res.isEmpty) {
          failures += classSymbol
        }

        res
      }


      val r = classes map { cs => getMatchingMethodIn(cs) } collect { case Some(cs) => cs }

      settings.ifVerbose {
        if (!failures.isEmpty) {
          reporter.warn("Failed to find method "+methodSymbol.fullName+" (type: "+methodSymbol.tpe+") in classes "+failures.map(_.name).mkString(","))
        }
      }
      r
    }


    val name = "Class Analysis"

    def run {
      // generating class blocks, and vertices
      funDecls.values.map(_.symbol).groupBy(_.owner).foreach { case (cl, mss) =>
        classAnalysisGraph addClass cl

        mss.foreach(m => classAnalysisGraph addMethod m)
      }

      for ((sym, f) <- funDecls) {
        analyze(f)
      }

      if (settings.dumpCallGraph) {
        val path = "callgraph.dot"
        reporter.info("Dumping Call Graph to "+path)
        new DotConverter(classAnalysisGraph, "Call Graph Analysis").toFile(path)
      }
    }
  }
}
