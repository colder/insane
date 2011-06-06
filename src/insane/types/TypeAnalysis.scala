package insane
package types

import utils._
import utils.Graphs._

trait TypeAnalysis {
  self: AnalysisComponent =>

  import global._

  case class TAVertex(symbol: Symbol) extends MutVertexAbs[EdgeSimple[TAVertex]] {
    val name = symbol.toString();
  }

  object TAUnknownTarget extends TAVertex(NoSymbol) {
    override val name = "?"
  }

  class CallGraph extends MutableDirectedGraphImp[TAVertex, EdgeSimple[TAVertex]] {
    var cToG = Map[Symbol, Group]()
    var mToV = Map[Symbol, TAVertex]()

    def addClass(s: Symbol): Group = {
      if (!(cToG contains s)) {
        val gr = new Group(s.toString(), RootGroup)
        addGroup(gr)
        cToG += s -> gr
      }
      cToG(s)
    }

    def addMethod(s: Symbol): TAVertex= {
      if (!(mToV contains s)) {
        val v = TAVertex(s)
        this += v
        mToV += s -> v

        inGroup(v, addClass(s.owner))
      }
      mToV(s)
    }

    def addMethodCall(from: Symbol, to: Symbol) {
      val vFrom = addMethod(from)
      val vTo   = addMethod(to)

      this += EdgeSimple[TAVertex](vFrom, vTo)
    }

    def addUnknownTarget(from: Symbol) {
      this += TAUnknownTarget
      this += EdgeSimple[TAVertex](addMethod(from), TAUnknownTarget)
    }

  }

  class TypeAnalysisPhase extends SubPhase {
    type ObjectInfo = ObjectSet

    object TypeAnalysisLattice extends dataflow.LatticeAbs[TypeAnalysisEnv, CFG.Statement] {
      val bottom = BaseTypeAnalysisEnv

      def join(envs: TypeAnalysisEnv*) = envs.toSeq.reduceLeft(_ union _)
    }

    class TypeAnalysisEnv(dfacts: Map[CFG.Ref, ObjectInfo]) extends dataflow.EnvAbs[TypeAnalysisEnv, CFG.Statement] {
      var isBottom = false

      var facts = dfacts

      def setFact(t : (CFG.Ref, ObjectInfo)) {
          facts += t
      }

      def getFact(r: CFG.Ref): ObjectInfo = facts.get(r) match {
        case Some(f) => f
        case None =>
          val fact = r match {
            case sr: CFG.SymRef if sr.symbol.isModule => // backpatching for Object.foo
              new ObjectSet(Set(sr.symbol.tpe), sr.symbol.tpe.typeSymbol.isFinal)
            case _ =>
              reporter.warn("Reference "+r+" not registered in facts at "+r.pos)
              new ObjectSet(Set(), false)
          }
          facts += r -> fact
          fact
      }

      def this() = this(Map());

      def duplicate = new TypeAnalysisEnv(facts)

      def union(that: TypeAnalysisEnv) = {
        var newFacts = Map[CFG.Ref, ObjectInfo]()

        for(k <- this.facts.keys ++ that.facts.keys) {
          newFacts += k -> (this.facts.getOrElse(k, ObjectSet.empty) ++ that.facts.getOrElse(k, ObjectSet.empty))
        }

        new TypeAnalysisEnv(newFacts)
      }

      override def equals(that: Any) = that match {
        case a: TypeAnalysisEnv =>
          (a.facts == facts) && (a.isBottom == isBottom)
        case _ => false
      }

      override def toString = { 
        (facts.map { case (k, v) => k+" => "+v } mkString("{", "; ", "}"))
      }
    }

    object BaseTypeAnalysisEnv extends TypeAnalysisEnv(Map()) {
      override def duplicate = new TypeAnalysisEnv(Map())
      override def union(that: TypeAnalysisEnv) = that

      override def equals(e: Any) = {
          if (e.isInstanceOf[AnyRef]) {
              BaseTypeAnalysisEnv eq e.asInstanceOf[AnyRef]
          } else {
              false
          }
      }

      override def toString = {
        "<base>"
      }

      isBottom = true
    }

    def getOSetFromRef(env: TypeAnalysisEnv, r: CFG.Ref): ObjectSet = r match {
      case th: CFG.ThisRef =>
        getDescendents(th.symbol)
      case su: CFG.SuperRef =>
        ObjectSet.singleton(su.symbol.superClass.tpe)
      case r =>
        env.getFact(r)
    }

    class TypeAnalysisTF extends dataflow.TransferFunctionAbs[TypeAnalysisEnv, CFG.Statement] {
      type Env = TypeAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.duplicate

        def getOSetFromSV(sv: CFG.SimpleValue) = sv match {
          case r2: CFG.Ref =>
            getOSetFromRef(env, r2)
          case n: CFG.Null =>
            ObjectSet.empty
          case _: CFG.StringLit =>
            ObjectSet.singleton(definitions.StringClass.tpe)
          case _: CFG.LiteralValue =>
            // irrelevant call
            ObjectSet.empty
        }

        st match {
          case (av: CFG.AssignVal) =>
            env setFact (av.r -> getOSetFromSV(av.v))

          case (afw: CFG.AssignFieldWrite) =>
            // ignore

          case (afr: CFG.AssignFieldRead) =>
            env setFact (afr.r -> getDescendents(afr.field))

          case (aa: CFG.AssignTypeCheck) =>
            // ignore, returns boolean

          case (aa: CFG.AssignArray) =>
            val newOset = ObjectSet(Set(typeRef(NoPrefix, definitions.ArrayClass, List(aa.tpe))), true)
            env setFact (aa.r -> newOset)

          case (aa: CFG.AssignCast) =>
            val oset = getOSetFromRef(env, aa.rhs)

            /**
             * If it's upcasting, we keep the most precise type as it's only
             * used for dynamic dispatch, on which we gain precision. If it's
             * downcasting, we keep the casted type and assume that the
             * compiler/code is correct.
             */
            val newOSet = if (oset.types.forall(t => t <:< aa.tpe)) {
              // upcasting
              oset
            } else if (oset.types.forall(t => aa.tpe <:< t)) {
              // down casting
              ObjectSet(Set(aa.tpe), oset.isExhaustive)
            } else {
              settings.ifVerbose {
                aa.tpe match {
                  case TypeRef(_, definitions.ArrayClass, _) =>
                    // For arrays, they are invariant for Scala, but not for scala, no error here.
                  case _ =>
                    reporter.warn("Cast is neither up or down: "+oset+".asInstanceof["+aa.tpe+"] at "+aa.getTree.pos)
                }
              }
              ObjectSet(Set(aa.tpe), oset.isExhaustive)
            }

            env setFact(aa.r -> newOSet)

          case aam: CFG.AssignApplyMeth =>
            if (isGroundClass(aam.meth.owner)) {
              env setFact(aam.r -> ObjectSet.empty)
            } else {
              // We check if we are in the special case array.apply() in which
              // case the type of the array elements are still stored in the
              // array type
              if (aam.meth.name.toString == "apply") {
                val objOSet    = getOSetFromSV(aam.obj)
                val objTypes   = objOSet.types
                val arrayTypes = objTypes collect { case TypeRef(_, definitions.ArrayClass, List(tpe)) => tpe }

                if (arrayTypes.size == objTypes.size) {
                  env setFact(aam.r -> ObjectSet(arrayTypes, objOSet.isExhaustive))
                } else {
                  env setFact(aam.r -> methodReturnType(aam.meth))
                }
              } else {
                env setFact(aam.r -> methodReturnType(aam.meth))
              }
            }
          case an: CFG.AssignNew =>
            // an.symbol is the constructor symbol
            env setFact (an.r -> ObjectSet.singleton(an.symbol.tpe))
          case CFG.Skip | _: CFG.Branch | _: CFG.AssertEQ | _ : CFG.AssertNE =>
            // ignored
        }

        env
      }
    }



    def analyze(f: AbsFunction) {
      val cfg       = f.cfg
      val bottomEnv = BaseTypeAnalysisEnv;
      val baseEnv   = new TypeAnalysisEnv();

      // We add conservative info about arguments in the class env
      for (a <- f.CFGArgs) {
        baseEnv setFact(a -> getDescendents(a.symbol))
      }


      val ttf = new TypeAnalysisTF
      val aa = new dataflow.Analysis[TypeAnalysisEnv, CFG.Statement](TypeAnalysisLattice, baseEnv, settings)
      if (settings.displayTypeAnalysis(f.symbol.fullName)) {
        reporter.info("Analyzing "+f.symbol.fullName+"...")
      }

      aa.computeFixpoint(cfg, ttf)

      if (settings.displayTypeAnalysis(f.symbol.fullName)) {
        if (settings.displayFixPoint) {
            println("     - Fixpoint:");
            for ((v,e) <- aa.getResult.toSeq.sortWith{(x,y) => x._1.name < y._1.name}) {
                println("      * ["+v+"] => "+e);
            }
        }
      }

      def methodCall(call: CFG.AssignApplyMeth, obj: CFG.Ref, oset: ObjectSet,  ms: Symbol) {

        if (oset.types.isEmpty) {
          settings.ifVerbose {
            reporter.warn("Empty object pool for "+obj+" with call to "+ms.name+" at "+call.pos)
          }
        }

        val matches = getMatchingMethods(ms, oset.types)

        if (settings.displayTypeAnalysis(f.symbol.fullName)) {
          reporter.info("Possible targets: "+matches.size +" "+(if (oset.isExhaustive) "bounded" else "unbounded")+" method: "+ms.name)
        }

        f.callTargets += call -> (matches, oset.isExhaustive)

        for (m <- matches) {
          callGraph.addMethodCall(f.symbol, m)
          simpleCallGraph        += (f.symbol -> (simpleCallGraph(f.symbol) + m))
          simpleReverseCallGraph += (m        -> (simpleReverseCallGraph(m) + f.symbol))
        }

        if (!oset.isExhaustive && !settings.wholeCodeAnalysis) {
          callGraph.addUnknownTarget(f.symbol)
        }
      }

      def generateResults(s: CFG.Statement, env: TypeAnalysisEnv) {
        s match {
          case aam: CFG.AssignApplyMeth =>
            if (!isGroundClass(aam.meth.owner)) {
              aam.obj match {
                case objref: CFG.Ref =>
                  aam.meth.tpe match {
                    case _: MethodType | _:PolyType | _:NullaryMethodType =>
                      methodCall(aam, objref, getOSetFromRef(env, objref), aam.meth)
                    case _ =>
                      reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+")")
                  }
                case _ =>
                // Ingore, <literal>.method()
              }

            }
          case _ => // ignore
        }
      }

      aa.pass(cfg, generateResults)
    }

    val name = "Class analysis and Call Graph generation"

    def run() {
      // 1) Generating class blocks, and vertices
      funDecls.values.map(_.symbol).groupBy(_.owner).foreach { case (cl, mss) =>
        callGraph addClass cl

        mss.foreach(m => callGraph addMethod m)
      }

      // 2) Add edges between methods
      for ((sym, f) <- funDecls) {
        analyze(f)
      }

      // 3) Generate SCC of the callGraph
      val scc = new StronglyConnectedComponents(callGraph)

      callGraphSCCs = scc.topSort(scc.getComponents)

      if (settings.dumpCallGraph) {
        val path = "callgraph.dot"
        reporter.info("Dumping Call Graph to "+path)
        new DotConverter(callGraph, "Call Graph Analysis").writeFile(path)

        println(callGraphSCCs)
      }
    }
  }
}
