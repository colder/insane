package insane
package types

import CFG.{CFGEdge,CFGVertex}

import utils._
import utils.Graphs._
import utils.Reporters._

trait TypeAnalysis {
  self: AnalysisComponent =>

  import global._

  case class TAVertex(symbol: Symbol) extends VertexAbs {
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
    object TypeAnalysisLattice extends dataflow.LatticeAbs[TypeAnalysisEnv] {
      val bottom = BaseTypeAnalysisEnv

      def join(envs: TypeAnalysisEnv*) = envs.toSeq.reduceLeft(_ union _)
    }

    class TypeAnalysisEnv(dfacts: Map[CFG.Ref, TypeInfo]) extends dataflow.EnvAbs[TypeAnalysisEnv] {
      var isBottom = false

      var facts = dfacts

      def setFact(t : (CFG.Ref, TypeInfo)) {
          facts += t
      }

      def getFact(r: CFG.Ref): TypeInfo = facts.get(r) match {
        case Some(f) => f
        case None =>
          reporter.warn("Reference "+r+" not registered in facts", r.pos)

          val fact = TypeInfo.empty
          facts += r -> fact

          fact
      }

      def this() = this(Map());

      def duplicate = new TypeAnalysisEnv(facts)

      def union(that: TypeAnalysisEnv) = {
        var newFacts = Map[CFG.Ref, TypeInfo]()

        for(k <- this.facts.keys ++ that.facts.keys) {
          newFacts += k -> (this.facts.getOrElse(k, TypeInfo.empty) union that.facts.getOrElse(k, TypeInfo.empty))
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
      override def duplicate = this
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

    // getOSetFromRef
    def getTpeInfoFromRef(env: TypeAnalysisEnv, r: CFG.Ref): TypeInfo = r match {
      case th: CFG.ThisRef =>
        TypeInfo.subtypeOf(th.symbol.tpe)
      case su: CFG.SuperRef =>
        TypeInfo.exact(su.symbol.superClass.tpe)
      case sr: CFG.ObjRef =>
        TypeInfo.exact(sr.symbol.tpe)
      case r =>
        env.getFact(r)
    }

    class TypeAnalysisTF extends dataflow.SimpleTransferFunctionAbs[TypeAnalysisEnv, CFG.Statement] {
      type Env = TypeAnalysisEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        var env = oldEnv.duplicate

        //getOSetFromSV
        def getTpeInfoFromSV(sv: CFG.SimpleValue) = sv match {
          case r2: CFG.Ref =>
            getTpeInfoFromRef(env, r2)
          case n: CFG.Null =>
            TypeInfo.empty
          case _: CFG.ByteLit =>
            TypeInfo.exact(definitions.ByteClass.tpe)
          case _: CFG.CharLit =>
            TypeInfo.exact(definitions.CharClass.tpe)
          case _: CFG.IntLit =>
            TypeInfo.exact(definitions.IntClass.tpe)
          case _: CFG.FloatLit =>
            TypeInfo.exact(definitions.FloatClass.tpe)
          case _: CFG.DoubleLit =>
            TypeInfo.exact(definitions.DoubleClass.tpe)
          case _: CFG.StringLit =>
            TypeInfo.exact(definitions.StringClass.tpe)
          case el: CFG.EnumLit =>
            TypeInfo.exact(el.tpe)
          case cl: CFG.ClassLit =>
            TypeInfo.exact(cl.tpe)
          case _: CFG.LiteralValue =>
            // irrelevant call
            TypeInfo.empty
        }

        st match {
          case (av: CFG.BasicBlock) =>
            var tmpEnv = env

            for (stmt <- av.stmts) {
              tmpEnv = apply(stmt, tmpEnv)
            }

            env = tmpEnv

          case (av: CFG.AssignVal) =>
            env setFact (av.r -> getTpeInfoFromSV(av.v))

          case (afw: CFG.AssignFieldWrite) =>
            // ignore

          case (afr: CFG.AssignFieldRead) =>
            env setFact (afr.r -> TypeInfo.subtypeOf(afr.field.tpe))

          case (aa: CFG.AssignTypeCheck) =>
            // ignore, returns boolean

          case (aa: CFG.AssignCast) =>
            val info = getTpeInfoFromRef(env, aa.rhs)

            val newInfo = aa.tpe match {
              case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
                TypeInfo.exact(aa.tpe)

              case tpe =>
                var isect = info.intersectWith(tpe)

                if (!isect.isEmpty) {
                  isect.get
                } else {
                  settings.ifDebug {
                    reporter.warn("Type intersection between "+info+" and "+tpe+" is empty! Falling back to cast type: "+tpe, aa.pos);
                  }

                  TypeInfo.subtypeOf(aa.tpe)
                }
            }

            env setFact(aa.r -> newInfo)

          case aam: CFG.AssignApplyMeth =>
            if (isGroundClass(aam.meth.tpe.resultType.typeSymbol)) {
              env setFact(aam.r -> TypeInfo.empty)
            } else {
              // We check if we are in the special case array.apply() in which
              // case the type of the array elements are still stored in the
              // array type
              if (aam.meth.name.toString == "apply") {
                val objTpe    = getTpeInfoFromSV(aam.obj)

                // Quick check to avoid resolving types for non-arrays
                if (objTpe.tpe match { case TypeRef(_, definitions.ArrayClass, List(tpe)) => true; case _ => false }) {
                  val objTypes   = objTpe.resolveTypes
                  val arrayTypes = objTypes collect { case TypeRef(_, definitions.ArrayClass, List(tpe)) => tpe }

                  if (arrayTypes.size == objTypes.size) {
                    env setFact(aam.r -> TypeInfo.fromTypes(objTypes, true))
                  } else {
                    env setFact(aam.r -> methodReturnType(aam.meth))
                  }
                } else {
                  env setFact(aam.r -> methodReturnType(aam.meth))
                }
              } else {
                env setFact(aam.r -> methodReturnType(aam.meth))
              }
            }
          case an: CFG.AssignNew =>
            // an.symbol is the constructor symbol
            env setFact (an.r -> TypeInfo.exact(an.tpe))
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
      for (a <- cfg.args) {
        baseEnv setFact(a -> TypeInfo.subtypeOf(a.tpe))
      }

      val ttf = new TypeAnalysisTF
      val aa = new dataflow.Analysis[TypeAnalysisEnv, CFG.Statement, FunctionCFG](TypeAnalysisLattice, baseEnv, settings, cfg)
      if (settings.displayTypeAnalysis(safeFullName(f.symbol))) {
        reporter.msg("Analyzing "+uniqueFunctionName(f.symbol)+"...")
      }

      //val name = safeFullName(f.symbol);
      //val dest = name+"-cfg.dot"
      //reporter.msg("Dumping CFG to "+dest+"...")
      //new CFGDotConverter(cfg, "CFG For "+name).writeFile(dest)

      aa.computeFixpoint(ttf)

      if (settings.displayTypeAnalysis(safeFullName(f.symbol))) {
        if (settings.displayFixPoint) {
            println("     - Fixpoint:");
            for ((v,e) <- aa.getResult.toSeq.sortWith{(x,y) => x._1.name < y._1.name}) {
                println("      * ["+v+"] => "+e);
            }
        }
      }

      def methodCall(call: CFG.AssignApplyMeth, obj: CFG.Ref, info: TypeInfo,  ms: Symbol) {

        if (info.resolveTypes.isEmpty) {
          settings.ifVerbose {
            reporter.warn("Empty object pool for "+obj+" with call to "+uniqueFunctionName(ms), call.pos)
          }
        }

        val typeMap = computeTypeMap(call.meth, call.typeArgs, info)

        val methodType = typeMap(ms.tpe)
        // In case of a dynamic call, we can expect lookup failures for some non-refined types
        val matches = getMatchingMethods(ms, methodType, info).map(_._1)

        if (call.isDynamic && matches.isEmpty) {
          reporter.warn("No method "+uniqueFunctionName(ms)+" found for call "+call+". Types: "+info, call.pos)
        }

        if (settings.displayTypeAnalysis(safeFullName(f.symbol))) {
          reporter.msg("Possible targets: "+matches.size +" "+(if (info.isExhaustive) "bounded" else "unbounded")+" method: "+ms.name)
        }

        f.callTargets += call -> (matches, info.isExhaustive)

        for (m <- matches) {
          callGraph.addMethodCall(f.symbol, m)
          simpleCallGraph        += (f.symbol -> (simpleCallGraph(f.symbol) + m))
          simpleReverseCallGraph += (m        -> (simpleReverseCallGraph(m) + f.symbol))
        }

        if (settings.dumpCallStats) {
          val refinedTargets = matches.size
          var allMatches = getMatchingMethods(ms, methodType, TypeInfo.subtypeOf(ms.owner.tpe)).size

          methodCallsStats += call.uniqueID -> (refinedTargets, allMatches)
        }

        if (!info.isExhaustive && !settings.assumeClosedWorld) {
          callGraph.addUnknownTarget(f.symbol)
        }
      }

      val generateResults = new dataflow.UnitTransferFunctionAbs[TypeAnalysisEnv, CFG.Statement] {
        def apply(e: CFG.Statement, env: TypeAnalysisEnv) = e match {
          case bb: CFG.BasicBlock =>
            bb.stmts.foreach(apply(_, env))

          case aam: CFG.AssignApplyMeth =>
            if (!isGroundClass(aam.meth.owner)) {
              aam.obj match {
                case objref: CFG.Ref =>
                  aam.meth.tpe match {
                    case _: MethodType | _:PolyType | _:NullaryMethodType =>
                      methodCall(aam, objref, getTpeInfoFromRef(env, objref), aam.meth)
                    case _ =>
                      reporter.warn("Unexpected type for method symbol: "+aam.meth.tpe+"("+aam.meth.tpe.getClass+")", aam.pos)
                  }
                case _ =>
                // Ingore, <literal>.method()
              }

            }
          case _ => // ignore
        }
      }

      aa.pass(generateResults)
    }

    val name = "Class analysis and Call Graph generation"

    def run() {
      if (settings.onDemandMode) {
        reporter.msg("Skipping type analysis and callgraph computations")
      } else {
        // 1) Generating class blocks, and vertices
        declaredFunctions.values.map(_.symbol).groupBy(_.owner).foreach { case (cl, mss) =>
          callGraph addClass cl

          mss.foreach(m => callGraph addMethod m)
        }

        // 2) Add edges between methods
        for ((sym, f) <- declaredFunctions) {
          analyze(f)
        }

        reporter.msg("Generating callgraph SCCs ("+callGraph.E.size+" edges for "+callGraph.V.size+" vertices)...")

        var tStart = System.currentTimeMillis

        // 3) Generate SCC of the callGraph
        val scc = new StronglyConnectedComponents(callGraph)


        val components = scc.getComponents

        settings.ifVerbose {
          reporter.msg("Finished ("+(System.currentTimeMillis-tStart)+"ms)")
          reporter.msg("Topsorting "+components.size+" SCCs...")
          tStart = System.currentTimeMillis
        }

        callGraphSCCs = scc.topSort(components)

        for (scc <- callGraphSCCs; sym <- scc.vertices.map(_.symbol)) {
          methCallSCC += sym -> scc 
        }

        reporter.msg("Finished ("+(System.currentTimeMillis-tStart)+"ms)")

        if (settings.dumpCallGraph) {
          val path = "callgraph.dot"
          reporter.msg("Dumping Call Graph to "+path)
          new DotConverter(callGraph, "Call Graph Analysis").writeFile(path)
        }

        if (settings.dumpCallStats) {
          import java.io.{BufferedWriter, FileWriter}
          val path = "callstats.data"
          val out = new BufferedWriter(new FileWriter(path))
          for ((_, (precise, all)) <- methodCallsStats) {
            out.write(precise+"\t"+all+"\n")
          }
          out.close()
        }
      }
    }
  }
}
