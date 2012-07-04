package insane
package alias

//import storage.Database

import utils._
import utils.Reporters._
import GlobalCounters.{withDebugCounter, getDebugCounter}
import utils.Graphs.DotConverter
import CFG._

import scala.tools.nsc.symtab.Flags

trait PointToAnalysis extends PointToGraphsDefs with PointToEnvs with PointToLattices {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  sealed abstract class PTAnalysisModes {

  }

  case object PreciseAnalysis        extends PTAnalysisModes
  case object BluntAnalysis          extends PTAnalysisModes
  case object ReductionAnalysis      extends PTAnalysisModes
  case object ConditionalAnalysis    extends PTAnalysisModes

  type AnalysisMode = PTAnalysisModes

  case class AnalysisFallbackException(fallBack: Int, reason: String) extends Exception

  object GiveUpException {
    def apply(reason: String) = AnalysisFallbackException(-1, reason)
  }

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    type PTDataFlowAnalysis = dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG]

    lazy val arrayStoreField = {
      GhostField(definitions.ArrayClass.newVariable(newTermName("_store_")), TypeInfo.subtypeOf(definitions.ArrayClass.typeParams(0).tpe))
    }


    var predefinedHighPriorityCFG = Map[Symbol, Option[FunctionCFG]]()
    def getPredefHighPriorityCFG(sym: Symbol) = {

      val AllScalaStubs = List(
        "scala\\.sys\\.error.*",
        "scala\\.reflect\\.ArrayTag\\.newArray.*",
        "scala\\.reflect\\.ClassTag\\.apply.*",
        "scala\\.Array\\.length.*",
        "scala\\.Array\\.<init>.*",
        "java\\.lang\\.Object\\..*",
        "scala\\.math\\.ScalaNumber\\..*",
//        "^scala\\.BoxesRunTime\\.hashFrom(Long|Double|Float|Number)\\..+",
        "java\\.lang\\.(?:Number|Float|Integer|Boolean|Character|Class|Double|Byte|Long)\\..*",
        "java\\..+Exception.*"
      ).mkString("|").r

      val ArrayUpdate = "scala\\.Array\\.update.*".r
      val ArrayApply  = "scala\\.Array\\.apply.*".r
      val RuntimeArrayApply  = "scala\\.runtime\\.ScalaRunTime\\.array_apply.*".r

      predefinedHighPriorityCFG.get(sym) match {
        case Some(optcfg) =>
          optcfg

        case None =>
          val optcfg = sym.fullName match {

            case name if settings.consideredPure(name) =>
              Some(buildPureEffect(sym))

            case AllScalaStubs() =>
              Some(buildPureEffect(sym))

            case _ if definitions.isScalaValueType(sym.owner.tpe) =>
              Some(buildPureEffect(sym))

            case ArrayUpdate() =>
              Some(buildEffect(sym){ (pureEffectBuilder _).andThen{ case ((cfg, env)) =>
                (cfg, env.write(env.locState(cfg.mainThisRef), arrayStoreField, env.locState(cfg.args(1)), allowStrongUpdates = false))
              }})

            case ArrayApply() =>
              Some(buildEffect(sym){ case ((cfg, env)) =>
                (cfg, env.read(env.locState(cfg.mainThisRef), arrayStoreField, cfg.retval, NoUniqueID))
              })

            case RuntimeArrayApply() =>
              Some(buildEffect(sym){ case ((cfg, env)) =>
                (cfg, env.read(env.locState(cfg.args(0)), arrayStoreField, cfg.retval, NoUniqueID))
              })

            case s =>
              None
          }

          predefinedHighPriorityCFG += sym -> optcfg

          optcfg
      }
    }


    var predefinedLowPriorityCFGs = Map[Symbol, Option[FunctionCFG]]()
    def getPredefLowPriorityCFG(sym: Symbol) = {

      predefinedLowPriorityCFGs.get(sym) match {
        case Some(optcfg) =>
          optcfg

        case None =>
          val optcfg = uniqueFunctionName(sym) match {
            case "java.lang.Object.<init>(()Object)" |
                 "java.lang.Object.$eq$eq((x$1: Object)Boolean)" |
                 "java.lang.Boolean.valueOf((x$1: Boolean)java.lang.Boolean)" |
                 "java.lang.Object.$bang$eq((x$1: Object)Boolean)" =>

              Some(buildPureEffect(sym))

            case _ =>
              None
          }

          predefinedLowPriorityCFGs += sym -> optcfg

          optcfg
      }
    }


    def getPTCFGFromFun(fun: AbsFunction): FunctionCFG = {
      getPTCFGFromFun(fun, TypeSignature.fromDeclaration(fun))
    }

    def getPTCFGFromFun(fun: AbsFunction, sig: TypeSignature): FunctionCFG = {
      getPTCFGResultFromFun(fun, sig)._1
    }


    private[this] def getPTCFGResultFromFun(initFun: AbsFunction, initSig: TypeSignature): (FunctionCFG, Boolean) = {
      val (fun, sig) = methodProxies.get(initFun.symbol) match {
        case Some(f) =>
          (f, initSig.convertForProxy(initFun.symbol, f))
        case None =>
          (initFun, initSig)
      }

      // Is the PTCFG for this signature already ready?
      fun.ptCFGs.get(sig) match {
        case Some(result) =>
          // Yes.
          result
        case None =>
          // No, we prepare a fresh one given the types, and store it.
          val result = (preparePTCFG(fun, sig), false)
          fun.ptCFGs += sig -> result
          result
      }
    }

    def getPTCFG(initSym: Symbol, initSig: TypeSignature): Option[FunctionCFG] = {
      val (sym, sig) = methodProxies.get(initSym) match {
        case Some(fun) =>
          (fun.symbol, initSig.convertForProxy(initSym, fun))
        case None =>
          (initSym, initSig)
      }


      getPredefHighPriorityCFG(sym) match {
        case Some(cfg) =>
          Some(cfg)
        case None =>
          lookupFunction(sym) match {
            case Some(fun) =>
              Some(getPTCFGFromFun(fun, sig))
            case None =>
              getPredefLowPriorityCFG(sym)
          }
      }
    }

    def getPTCFGAnalyzed(initSym: Symbol, initSig: TypeSignature): Option[FunctionCFG] = {
      val (sym, sig) = methodProxies.get(initSym) match {
        case Some(fun) =>
          (fun.symbol, initSig.convertForProxy(initSym, fun))
        case None =>
          (initSym, initSig)
      }

      getPredefHighPriorityCFG(sym) match {
        case Some(cfg) =>
          Some(cfg)
        case None =>
          lookupFunction(sym) match {
            case Some(fun) =>
              Some(getPTCFGAnalyzedFromFun(fun, sig))
            case None =>
              getPredefLowPriorityCFG(sym)
          }
      }
    }

    private[this] def getPTCFGAnalyzedFromFun(fun: AbsFunction, sig: TypeSignature): FunctionCFG = {
      getPTCFGResultFromFun(fun, sig) match {
        case (cfg, true) =>
          cfg
        case (cfg, false) =>
          specializedAnalyze(fun, PreciseAnalysis, sig)
      }
    }

    def getFlatPTCFG(initSym: Symbol, initSig: TypeSignature): Option[FunctionCFG] = {
      val (sym, sig) = methodProxies.get(initSym) match {
        case Some(fun) =>
          (fun.symbol, initSig.convertForProxy(initSym, fun))
        case None =>
          (initSym, initSig)
      }

      val res = getPredefHighPriorityCFG(sym) match {
        case Some(cfg) =>
          Some(cfg)
        case None =>
          lookupFunction(sym) match {
            case Some(fun) =>
              fun.flatPTCFGs.get(sig) match {
                case Some(flatPTCFG) =>
                  // Already here? Nice.
                  Some(flatPTCFG)
                case _ =>
                  // We need to re-analyze in blunt-mode
                  var changed = false;

                  reporter.incIndent()
                  settings.ifDebug {
                    reporter.info("Performing blunt fixpoint on "+sym.fullName+" with signature: "+sig)
                  }

                  // We prevent infinite recursion by assigning a bottom effect by default.
                  fun.flatPTCFGs += sig -> constructFlatCFG(fun, getPTCFGFromFun(fun, sig), EmptyPTEnv)

                  def computeFlatEffect() = {
                    // Here the receiver might be a supertype of the method owner, we restrict in this case:

                    val cfg = specializedAnalyze(fun, BluntAnalysis, sig)

                    assert(cfg.isFlat, "CFG Returned is not Flat!")

                    val effect = cfg.graph.E.head.label match {
                      case e: CFGTrees.Effect =>
                        e.env
                      case _ =>
                        sys.error("Flat CFG does not contain edge with Effects!")
                    }

                    (cfg, effect)
                  }

                  val tStart = System.currentTimeMillis

                  var pass = 1

                  val cnt = getDebugCounter

                  try {

                    var (oldCFG, oldEffect) = computeFlatEffect()

                    settings.ifDebug {
                      dumpCFG(oldCFG, "fix-"+cnt+"-"+pass+".dot")
                    }

                    do {
                      var (newCFG, newEffect) = computeFlatEffect()

                      //val joinEffect = PointToLattice.join(oldEffect, newEffect)

                      pass += 1;

                      settings.ifDebug {
                        dumpCFG(newCFG, "fix-"+cnt+"-"+pass+".dot")
                      }

                      changed = (newEffect != oldEffect)
                      if (pass > 30 && changed) {
                        //reporter.debug(" Before : =================================")
                        //reporter.debug(oldEffect.toString)
                        //reporter.debug(" After  : =================================")
                        //reporter.debug(newEffect.toString)
                        //reporter.debug(" Diff   : =================================")
                        //oldEffect diffWith newEffect
                        throw GiveUpException("Blunt-fixpoint taking more than 30 passes")
                      }
                      oldCFG    = newCFG;
                      oldEffect = newEffect;
                    } while(changed);

                    reporter.decIndent()

                    fun.flatPTCFGs     += sig -> oldCFG
                    fun.flatPTCFGsTime += sig -> (fun.flatPTCFGsTime(sig) + (System.currentTimeMillis - tStart))

                    settings.ifDebug {
                      reporter.msg("======= Analyzing "+sym.fullName+" required "+pass+" passes!")
                    }
                    Some(oldCFG)

                  } catch {
                    case e =>
                      reporter.decIndent()
                      throw e
                  }
              }
            case None =>
              getPredefLowPriorityCFG(sym)
          }
      }

      res match {
        case Some(ptCFG) =>
          assert(ptCFG.isFlat, "Reduced CFG obtained is not actually flat")
          Some(ptCFG)
        case None =>
          None
      }
    }


    def globalTick() {
      if (analysisStack.size > 0 && analysisStack.top.timeSpent() > settings.frameTimeout) {
        throw GiveUpException("Frame Timeout reached")
      }

      if ((System.currentTimeMillis - globalTStart) > settings.globalTimeout) {
        throw GiveUpException("Global Timeout reached")
      }
    }



    class PointToTF(fun: AbsFunction, analysisMode: AnalysisMode) extends dataflow.TransferFunctionAbs[PTEnv, CFG.Statement] {

      var analysis: PTDataFlowAnalysis = null

      def isRecursive(aam: CFG.AssignApplyMeth, symbol: Symbol, sig: TypeSignature) = {
        if (settings.onDemandMode) {
          if (recursiveMethods contains ((symbol, sig))) {
            true
          } else if (analysisStackSet contains ((symbol, sig))) {
            recursiveMethods += ((symbol, sig))
            true
          } else if (aam.inlinedIn contains ((symbol, sig))) {
            recursiveMethods += ((symbol, sig))
            true
          } else {
            false
          }
        } else {
          (simpleCallGraph(symbol) contains symbol) || ((methCallSCC contains symbol) && methCallSCC(symbol).size > 1)
        }
      }

      /*
       * Heuristic to decide to use flat effects to inline symbol.
       * The return value is used as follows:
       *  - true:  we obtain the definite effect from that call by
       *           possibly reanalyzing it and inline this flat effect.
       *  - false: we inline by CFG
       */
      def shouldUseFlatInlining(env: PTEnv, aam: CFG.AssignApplyMeth, target: Symbol, sig: TypeSignature): Boolean = {
        if (isRecursive(aam, target, sig)) {
          if (settings.onDemandFunction(safeFullName(fun.symbol))) {
            true
          } else {
            var newEnv = env

            def getNodes(a: CFG.SimpleValue): Set[Node] = {
              val (e, ns) = newEnv.getNodes(a)
              newEnv = e
              ns
            }

            val nodes = (getNodes(aam.obj) ++ aam.args.flatMap(getNodes)).filterNot(n => isGroundTypeInfo(n.types))

            //reporter.msg("Call: "+aam)
            if (newEnv.nodesEscape(nodes.toSet)) {
              settings.ifDebug {
                reporter.debug("YOH. it escapes...")
                withDebugCounter { cnt =>
                  dumpPTE(env, "yoh"+cnt+".dot");
                }
              }
              false
            } else {
              // If nothing of interest does escape, we can try to inline it now
              settings.ifDebug {
                reporter.debug("YAY. found a case!")
                withDebugCounter { cnt =>
                  dumpPTE(env, "yay"+cnt+".dot");
                }
              }
              true
            }
          }
        } else {
          false
        }
      }

      abstract class InlineStrategy {
        def shouldDelay(targetsToConsider: Set[UnresolvedTargetInfo],
                        currentFun: AbsFunction): Option[String];
      }

      object InlineStrategies {
        case object Smart extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[UnresolvedTargetInfo],
                          currentFun: AbsFunction): Option[String] = {

            val score = targetsToConsider.size

            if (score > settings.maxInlinableTargets) {
              Some("too many targets: "+score+" > "+settings.maxInlinableTargets)
            } else {
              None // No delaying
            }
          }
        }

        case object AlwaysInline extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[UnresolvedTargetInfo],
                          currentFun: AbsFunction): Option[String] = {

            None // No delaying
          }
        }

        case object AlwaysDelay extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[UnresolvedTargetInfo],
                          currentFun: AbsFunction): Option[String] = {

            if (settings.onDemandFunction(safeFullName(currentFun.symbol))) {
              None // No delaying as we are in one of the main functions
            } else {
              Some("Strategy requires us to delay")
            }
          }
        }
      }

      def getInlineStrategy: InlineStrategy = {
        settings.inlineStrategy match {
          case settings.InlineStrategies.Smart =>
            InlineStrategies.Smart

          case settings.InlineStrategies.AlwaysDelay =>
            InlineStrategies.AlwaysDelay

          case settings.InlineStrategies.AlwaysInline =>
            InlineStrategies.AlwaysInline
        }
      }

      def shouldWeInlineThis(env: PTEnv,
                             aam: CFG.AssignApplyMeth,
                             targets: Set[UnresolvedTargetInfo]): Either[(Set[ResolvedTargetInfo], AnalysisMode), (String, Boolean, Boolean)] = {

        val symbol            = aam.meth
        val excludedTargets   = aam.excludedSymbols
        // We combine the call signature of similar targets
        val targetsToConsider = targets.groupBy(_.sym).map{ case (sym, urs) => UnresolvedTargetInfo(sym, urs.map(_.sig).reduce(_ combine _)) }.toSet.filter(t => !excludedTargets(t.sym))

        analysisMode match {
          case PreciseAnalysis | ConditionalAnalysis =>
            val result = if (targets.isEmpty) {
              Right("no target could be found", true, true)
            } else {
              val receiverTypes  = (TypeInfo.empty /: targets) (_ union _.sig.rec.info)

              if (!receiverTypes.isExhaustive && !settings.assumeClosedWorld) {
                Right("unbouded number of targets", false, true)
              } else {
                val strategy = getInlineStrategy
                val optDelayReason = strategy.shouldDelay(targetsToConsider, fun)

                if (!optDelayReason.isEmpty) {
                  Right(optDelayReason.get, false, true)
                } else {
                  var targetsRecursive  = false
                  var maxfallback       = -1;
                  var targetsArbitrary  = false
                  var missingTargets = Set[Symbol]()

                  val availableTargets = targetsToConsider flatMap { case UnresolvedTargetInfo(sym, sigPrecise) =>

                    val sigUsed      = if (settings.contSenWhenPrecise) {
                      if (settings.contSenDepthMax > settings.contSenDepthWhenPrecise) {
                        sigPrecise.limitDepth(settings.contSenDepthWhenPrecise)
                      } else {
                        sigPrecise
                      }
                    } else {
                      TypeSignature.fromDeclaration(sym)
                    }

                    val optCFG = if (shouldUseFlatInlining(env, aam, sym, sigUsed)) {
                      // If we use flat inlining, we use the most precise type sig as possible
                      getFlatPTCFG(sym, sigPrecise)
                    } else if (isRecursive(aam, sym, sigUsed)) {
                      // In case we can't use flat inlining, we prevent
                      // inlining in case it is a recursive call.

                      // We are potentially at the end of the loop in the lasso-shaped callgraph
                      // we look at the stack to see where we can directly fall back to, that is the entry point of the lasso
                      val callChain = analysisStack.toList.map(ctx => (ctx.cfg.symbol, ctx.sig)).zipWithIndex;

                      callChain.find{ case (cc, i) => cc == (sym, sigUsed) } match {
                        case Some((cc, i)) =>
                          maxfallback = maxfallback max (i+1)
                        case _ =>
                      }

                      targetsRecursive = true
                      None
                    } else {
                      getPTCFGAnalyzed(sym, sigUsed)
                    }

                    optCFG match {
                      case _ if settings.consideredArbitrary(safeFullName(sym)) =>
                        targetsArbitrary = true
                        None

                      case None =>
                        missingTargets += sym
                        None

                      case Some(cfg) if cfg.isBottom =>
                        None
                      case Some(cfg) =>
                        Some(ResolvedTargetInfo(cfg, sigUsed))
                    }
                  }

                  if (targetsRecursive) {
                    if (maxfallback> 0) {
                      // GIVE UP
                      throw new AnalysisFallbackException(maxfallback, "Outer-recursive call detected")
                    } else {
                      Right("Recursive calls should stay as-is in precise mode", false, true)
                    }
                  } else if (targetsArbitrary) {
                    Right("Some targets are to be considered arbitrarily", false, true)
                  } else if (!missingTargets.isEmpty) {
                    Right("some targets are unanalyzable: "+missingTargets.map(uniqueFunctionName(_)).mkString(", "), true, true)
                  } else {
                    preciseCallTargetsCache += aam -> availableTargets

                    Left((availableTargets, if (availableTargets forall (_.cfg.isFlat)) BluntAnalysis else PreciseAnalysis))
                  }
                }
              }
            }

            if (analysisMode == ConditionalAnalysis && result.isRight) {
              Left(Set(ResolvedTargetInfo(buildPureEffect(aam.meth), TypeSignature.fromDeclaration(aam.meth))), BluntAnalysis)
            } else {
              result
            }
          case BluntAnalysis =>
            // We have to analyze this, and thus inline, no choice
            // here, we require that the result is a flat effect
            var missingTargets = Set[Symbol]()

            if (targetsToConsider.size > 50) {
              throw GiveUpException("Overly imprecise call: "+targetsToConsider.size+" targets");
            }

            val targetsCFGs = targetsToConsider flatMap { case UnresolvedTargetInfo(sym, sigPrecise) =>

              if (settings.consideredArbitrary(safeFullName(sym))) {
                  missingTargets += sym
                  None
              } else {
                getFlatPTCFG(sym, sigPrecise) match {
                  case None =>
                    missingTargets += sym
                    None
                  case Some(cfg)=>
                    Some(ResolvedTargetInfo(cfg, sigPrecise))
                 }
              }
            }

            preciseCallTargetsCache += aam -> targetsCFGs

            if (targetsToConsider.isEmpty) {
              Left((Set(), BluntAnalysis))
            } else if (targetsCFGs.isEmpty) {
              Right(("Some targets are missing: "+missingTargets.map(uniqueFunctionName(_)).mkString(", "), true, false))
            } else {
              Left((targetsCFGs, BluntAnalysis))
            }

          case ReductionAnalysis =>
            if (preciseCallTargetsCache contains aam) {
              Left((preciseCallTargetsCache(aam), BluntAnalysis))
            } else {
              Right(("Could not reduce since targets are not available!", true, false))
            }
        }
      }

      def apply(st: CFG.Statement, oldEnv: PTEnv, edge: Option[CFGEdge[CFG.Statement]]): PTEnv = {
        if (oldEnv.category.isBottom || oldEnv.category.isTop) {
          return oldEnv
        }

        globalTick()

        var env = oldEnv

        case class NodeMap(map: Map[Node, Set[Node]] = Map().withDefaultValue(Set())) extends Function1[Node, Set[Node]] {

          override def toString() = map.toString()

          def apply(n: Node): Set[Node] = map(n)

          def -(node: Node) = {
            copy(map = map - node)
          }

          def +(ns: (Node, Node)) = {
            copy(map = map + (ns._1 -> (map(ns._1)++Set(ns._2))))
          }

          def ++(ns: (Node, Set[Node])) = {
            if (ns._2.isEmpty) {
              reporter.error("Cannot add an empty mapping for "+ns._1)
              assert(false);
              this
            } else {
              copy(map = map + (ns._1 -> (map(ns._1) ++ ns._2)))
            }
          }

          def +++(ns: Traversable[(Node, Set[Node])]) = {
            copy(map = map ++ (ns.map(nn => (nn._1 -> (map(nn._1) ++ nn._2)))))
          }
        }

        def refineNode(outerG: PTEnv, innerNode: Node, outerNode: Node): (PTEnv, Node) = {
          (innerNode, outerNode) match {
            case (in, on) if (!in.isResolved && !on.isResolved) && (in.types isMorePreciseThan on.types) =>
              (in.types intersectWith on.types) match {
                case Some(tpe) =>
                  val newNode = on.withTypes(tpe)

                  val newOuterG = outerG.splitNode(on, newNode)
                  (newOuterG, newNode)
                case None =>
                  // If we can't, return the old anyway, it should be cleaned later on
                  (outerG, outerNode)
              }
            case _ =>
              // We can keep the old outerNode, and the env didn't change
              (outerG, outerNode)
          }
        }

        def mergeGraphs(outerG: PTEnv, innerG: PTEnv, uniqueID: UniqueID, pos: Position, allowStrongUpdates: Boolean): PTEnv = {
          if (innerG.category.isTop) {
            innerG
          } else if (outerG.category.isTop) {
            outerG
          } else if (outerG.category.isEmpty) {
            innerG
          } else if(innerG.category.isEmpty) {
            outerG
          } else {
            /**
             * In a standard merge graph (e.g. inlining local effects), we map
             * local variables exactly, once those are mapped correctly, we
             * proceed with mergeGraph as usual.
             */

            var newOuterG = outerG;
            var nodeMap   = NodeMap();

            for (innerNode <- innerG.ptGraph.V.collect{ case lv: LVNode => lv }) {
              val (newEnv, outerNodes) = newOuterG.getNodes(innerNode.ref);

              newOuterG = newEnv

              nodeMap ++= innerNode -> outerNodes
              //for (outerNode <- outerNodes) {
              //  // Refine the node in case inner <:< outer
              //  val (tmpEnv, newOuterNode) = refineNode(newOuterG, innerNode, outerNode);
              //  newOuterG = tmpEnv
              //  nodeMap += (innerNode -> newOuterNode)

              //  if (innerNode.types isStrictlyMorePreciseThan newOuterNode.types) {
              //    reporter.warn(" AAAAAAAAA "+innerNode+" --> "+newOuterNode)
              //  }
              //}
            }

            var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, uniqueID, pos, allowStrongUpdates)

            for ((r, nodes) <- innerG.locState) {
              val outerNodes = nodes flatMap newNodeMap
              if (outerNodes.isEmpty) {
                settings.ifDebug {
                  reporter.warn("Unable to map local variable "+r+" in the outer environment")
                  withDebugCounter { cnt =>
                    dumpPTE(innerG,     "err-"+cnt+"-in.dot")
                    dumpPTE(newOuterG,  "err-"+cnt+"-bef.dot")
                    dumpPTE(newOuterG2, "err-"+cnt+"-aft.dot")
                  }
                  for ((f, tos) <- newNodeMap.map) {
                    reporter.debug("  "+f+" -> "+tos)
                  }
                }
              } else {
                newOuterG2 = newOuterG2.setL(r, outerNodes)
              }
            }

            //withDebugCounter { cnt => 
            //  dumpPTE(newOuterG2, "result"+cnt+".dot")
            //}
            newOuterG2
          }
        }

        def mergeGraphsWithMap(outerG: PTEnv, innerG: PTEnv, nodeMapInit: NodeMap, uniqueID: UniqueID, pos: Position, allowStrongUpdates: Boolean): (PTEnv, NodeMap) = {
          // Build map
          var newOuterG = outerG;
          var nodeMap   = nodeMapInit;

          // 1) We add nodes that are globally reachable
          for (n <- innerG.ptGraph.V.filter(n => n.isInstanceOf[GloballyReachableNode] || n.isInstanceOf[SimpleNode])) {
            nodeMap += n -> n
            newOuterG = newOuterG.addNode(n)
          }

          // 4) Inline Inside nodes with refinement of the allocation site
          def inlineINode(iNode: INode): INode = {
            // 1) we compose a new unique id
            val callId = uniqueID

            val newId = iNode.pPoint safeAdd callId

            // Like before, we check if the node was here
            val iNodeUnique    = INode(newId, true,  iNode.sym)
            val iNodeNotUnique = INode(newId, false, iNode.sym)

            if (newOuterG.ptGraph.V contains iNodeNotUnique) {
              iNodeNotUnique
            } else if (newOuterG.ptGraph.V contains iNodeUnique) {
              newOuterG = newOuterG.replaceNode(iNodeUnique, Set(iNodeNotUnique))
              iNodeNotUnique
            } else if (!iNode.sgt) {
              newOuterG = newOuterG.addNode(iNodeNotUnique)
              iNodeNotUnique
            } else {
              newOuterG = newOuterG.addNode(iNodeUnique)
              iNodeUnique
            }
          }

          // Map all inside nodes to themselves
          nodeMap +++= innerG.ptGraph.vertices.collect{ case n: INode => (n: Node,Set[Node](inlineINode(n))) }

          // 5) Resolve load nodes
          def resolveLoadNode(lNode: LNode): Set[Node] =
            resolveLoadNodeSafe(lNode, Set(lNode))

          def resolveLoadNodeSafe(lNode: LNode, stack: Set[LNode]): Set[Node] = {
            globalTick()

            val LNode(_, field, pPoint, sig) = lNode
            //reporter.incIndent()
            //reporter.debug("Resolving "+lNode+" ("+stack+")")

            val innerFromNodes = innerG.ptGraph.ins(lNode).collect{ case OEdge(f, _, _) => f }

            //reporter.debug(" inner From:"+innerFromNodes)

            val fromNodes = innerFromNodes map ( n => (n, n match {
              case from : LNode if !stack(from) =>
                resolveLoadNodeSafe(from, stack + lNode)
              case from =>
                nodeMap(from)
            }))

            //reporter.debug(" mapped From:"+fromNodes)

            var pointedResults = Set[Node]()

            for ((innerFromNode, outerFromNodes) <- fromNodes; outerFromNode <- outerFromNodes) {

              val node = outerFromNode
              //val (tmpOuterG, node) = refineNode(newOuterG, innerFromNode, tmpNode)

              //if (node != tmpNode) {
              //  // Node was refined, we need to affect
              //  nodeMap += innerFromNode -> node
              //}

              //newOuterG = tmpOuterG

              //val writeTargets = newOuterG.getWriteTargets(Set(node), field)

              //var pointed = if (writeTargets.isEmpty) {
              //  newOuterG.getReadTargets(Set(node), field)
              //} else {
              //  writeTargets
              //}

              var pointed = newOuterG.getAllTargets(Set(node), field)
              //reporter.debug(" From "+node+" via "+field+" ==> "+pointed)

              // Filter only compatible point results:
              pointed = pointed.filterNot { _ match {
                  case ln: LNode =>
                    ln.types incompatibleWith lNode.types
                  case lv : LVNode =>
                    lv.types incompatibleWith lNode.types
                  case _ =>
                    false
                }
              }

              val shouldCreate = pointed.isEmpty

              if (shouldCreate) {
                node match {
                  case i: INode =>
                    /**
                     * this loadNode is mapped to an insideNode, innerG must
                     * have a write target for it, it will be brough to
                     * newOuterG later
                     */
                  case _ =>
                    val newId = pPoint safeAdd uniqueID

                    field.accessFromNode(node) match {
                      case Some(sig) =>
                        val newLNode = safeTypedLNode(sig, node, field, newId)

                        //for (nodeToAdd <- findSimilarLNodes(newLNode, newOuterG.ptGraph.V)) {
                        //  newOuterG = newOuterG.addNode(nodeToAdd).addOEdge(node, field, nodeToAdd)
                        //  pointedResults += nodeToAdd
                        //}
                        //reporter.debug("Creating new LNode")
                        newOuterG = newOuterG.addNode(newLNode).addOEdge(node, field, newLNode)
                        pointedResults ++= (pointed + newLNode)
                      case None =>
                       //reporter.debug("It is apparently impossible to reach field "+field+" from "+node)
                       // apparently impossible, ignore
                    }
                }
              } else {
                pointedResults ++= pointed
              }
            }

            //reporter.debug("   -> "+pointedResults)
            //reporter.decIndent()

            pointedResults
          }

          // Filter node mappings with incoherent types
          def removeInconsistencies() {
            for ((innerNode, outerNodes) <- nodeMap.map) {

              var toRemove = Set[Node]()
              for (outerNode <- outerNodes) {
                if (outerNode.types incompatibleWith innerNode.types) {
                  toRemove += outerNode
                }
              }

              if (!toRemove.isEmpty) {
                settings.ifDebug {
                  reporter.debug("Removing inconsistent nodes: "+toRemove)
                }
                nodeMap = nodeMap.copy(map = nodeMap.map + (innerNode -> (outerNodes--toRemove)))
              }

            }
          }

          // 6) Apply inner edges
          def applyInnerEdgesFixPoint(envInner: PTEnv, envInit: PTEnv, nodeMap: NodeMap): PTEnv = {
            var env  = envInit
            var lastEnv  = env

            do {
              lastEnv  = env

              // We map all edges to their new nodes potentially creating more or less edges
              val mappedEdges = for (IEdge(v1, field, v2) <- envInner.iEdges; mappedV1 <- nodeMap(v1); mappedV2 <- nodeMap(v2)) yield (IEdge(mappedV1, field, mappedV2), v1)

              for (((newV1, field), edgesOldV1) <- mappedEdges.groupBy { case (edge, oldV1) => (edge.v1, edge.label) }) {
                val (edges, oldV1s) = edgesOldV1.unzip

                // We only allow strong updates if newV1 was the only target of oldV1
                val allowStrong = allowStrongUpdates && oldV1s.forall { nodeMap(_).size == 1 }

                //println("writing to "+newV1+"."+field.name+" = "+ edges.map(_.v2))

                env = env.write(Set(newV1), field, edges.map(_.v2), allowStrong)
              }
            } while (lastEnv != env)

            env
          }

          var oldNodeMap = nodeMap
          var oldOuterG  = newOuterG


          var pass = 0
          do {
            pass += 1

            oldOuterG  = newOuterG
            oldNodeMap = nodeMap

            //reporter.debug("Map before OEdges: "+nodeMap)

            for (oe @ OEdge(v1, lab, v2) <- innerG.oEdges) {
              //reporter.debug(" Handling "+oe)
              //reporter.debug(" Before Map "+nodeMap)
              val ov1 = v1 match {
                case l: LNode =>
                  val res = resolveLoadNode(l)
                  if (!res.isEmpty) {
                    nodeMap ++= l -> res
                  }
                  res
                case n =>
                  nodeMap(n)
              }

              val ov2 = v2 match {
                case l: LNode =>
                  val res = resolveLoadNode(l)
                  if (!res.isEmpty) {
                    nodeMap ++= l -> res
                  }
                  res
                case n =>
                  nodeMap(n)
              }
              //reporter.debug(" After Map "+nodeMap)

              (ov1, ov2) match {
                case (v1s, v2s) if !v1s.isEmpty && !v2s.isEmpty =>
                  val fv1s = v1s.filter(lab.existsFromNode _)
                  if (!fv1s.isEmpty) {
                    newOuterG = newOuterG.addOEdges(fv1s, lab, v2s)
                  }
                case _ =>
                  // This will occur in case one of the nodes were resolved to a INode
                  // => We wait, once the IEdges are merged in, the OEdges should
                  // find valid sources and targets.
              }

            }

            //reporter.debug("Map After OEdges: "+nodeMap)

            newOuterG = applyInnerEdgesFixPoint(innerG, newOuterG, nodeMap)

            if (pass > 100) {
              reporter.fatal("Abording apparently dead fixpoint")
            } else if (pass >= 10 && pass <= 13) {
              settings.ifDebug {
                reporter.debug("Merge fixpoint taking more than 10 steps: "+pass+"?")
                withDebugCounter { cnt =>
                  dumpPTE(oldOuterG, "old-"+cnt+".dot")
                  dumpPTE(newOuterG, "new-"+cnt+".dot")
                }
              }
            }

          } while((oldOuterG != newOuterG) || (oldNodeMap != nodeMap))

          (newOuterG, nodeMap)
        }

        st match {
          case ef: CFG.Effect =>
            env = mergeGraphs(env, ef.env, ef.uniqueID, ef.pos, true)

          case av: CFG.AssignVal =>
            val (newEnv, nodes) = env.getNodes(av.v)
            env = newEnv.setL(av.r, nodes)

          case afr: CFG.AssignFieldRead =>

            val field = Field(afr.field)

            val (newEnv, fromNodes) = env.getNodes(afr.obj)

            env = newEnv.read(fromNodes, field, afr.r, afr.uniqueID)

          case afw: CFG.AssignFieldWrite =>
            val field = Field(afw.field)

            val (newEnv,  fromNodes) = env.getNodes(afw.obj)
            val (newEnv2, toNodes)   = newEnv.getNodes(afw.rhs)

            env = newEnv2.write(fromNodes, field, toNodes, true)

          case aam: CFG.AssignApplyMeth => // r = o.v(..args..)

            var (newEnv, nodes)   = env.getNodes(aam.obj)

            if (nodes.isEmpty) {
              val dest = "err-ptcfg.dot"
              new CFGDotConverter(analysis.cfg, "Point-to-CFG").writeFile(dest)
            }

            val name = uniqueFunctionName(fun.symbol);

            /**
             * We define two types of inlining:
             *  1) Inlining by CFG (Precise)
             *      When possible, we inline the CFG of the target method, or
             *      partial effect summary into the current CFG. This is
             *      generally not possible if there are mutually recursive
             *      functions.
             *  2) Inlining by Effects (Blunt)
             *      When inlining CFGs is not possible, we inline the effects
             *      directly. A definite effect, often imprecise, is computed.
             */

            var style = aam.style;


            val recSigs = aam.obj match {
              //case CFG.SuperRef(sym, _, tpe) =>
              //  // XXX: Change style here to static ??
              //  // For super calls, we have only one merged receiver signature/types

              //  val objTpe = (TypeInfo.empty /: nodes) (_ union _.types)
              //  val res = TypeInfo.exact(tpe.asSeenFrom(objTpe.tpe, tpe.typeSymbol))

              //  Set(SigEntry.fromTypeInfo(res))
              case _ =>
                nodes.map(n => typeSignatureFromNodes(newEnv, Set(n), settings.contSenDepthMax)).toSet
            }

            val callArgsSigs = for (a <- aam.args) yield {
              val (tmp, nodes) = newEnv.getNodes(a)
              newEnv = tmp
              typeSignatureFromNodes(newEnv, nodes, settings.contSenDepthMax)
            }

            val callSigs = for (recSig <- recSigs) yield {
              val typeMap = computeTypeMap(aam.meth, aam.typeArgs, recSig.info)

              TypeSignature(recSig, callArgsSigs, typeMap)
            }


            if (nodes.isEmpty) {
              dumpAnalysisStack()
              reporter.fatal("IMPOSSIBRU! Could not find any node for the receiver of: "+aam)
            }

            var targets: Set[UnresolvedTargetInfo] = callSigs.flatMap( sig => getPredefHighPriorityCFG(aam.meth) match {
              case Some(x) =>
                Some(UnresolvedTargetInfo(aam.meth, sig))
              case None =>
                getMatchingMethods(aam.meth, style, sig)
            })

            if (!targets.filterNot(ut => aam.excludedSymbols(ut.sym)).isEmpty) {
              settings.ifDebug {
                reporter.debug("Currently handling: "+aam)
                reporter.debug("  Sigs:      ")
                for (sig <- callSigs) {
                  reporter.debug("   -> "+sig)
                }
                //reporter.debug("  Map:      "+typeMap)
                reporter.debug("  Meth:     "+aam.meth.fullName)
                //for (t <- info.exactTypes) {
                //  reporter.debug("  For "+t)
                //  reporter.debug("   -> "+t.typeSymbol.tpe)
                //  reporter.debug("   -> "+t.typeSymbol.tpe.typeArgs)
                //}
                //reporter.debug("  Meth Own: "+aam.meth.owner)
                //reporter.debug("  Meth IsAbstract: "+aam.meth.isDeferred)
                //reporter.debug("  Meth O.T.:"+aam.meth.owner.tpe.typeArgs)
                //reporter.debug("  Raw Meth Tpe: "+aam.meth.tpe)
                //reporter.debug("  Map Meth Tpe: "+methodType)
                //reporter.debug("  Receiver: "+aam.obj+": (nodes: "+nodes+") "+info)
                //reporter.debug("  Analysis Mode:   "+analysisMode)
                //reporter.debug("  Inline Strategy: "+getInlineStrategy)
              }
              settings.ifDebug {
                reporter.debug("  Targets("+targets.size+") :")
                for (UnresolvedTargetInfo(sym, sig) <- targets.slice(0, 10)) {
                  reporter.debug("    -> "+sym.fullName +" with signature: "+sig)
                }
                if (targets.size > 10) {
                  reporter.debug("    -> ...")
                }
                reporter.debug("  Excluded Targets("+aam.excludedSymbols.size+") :")
                for (sym <-aam.excludedSymbols.slice(0, 10)) {
                  reporter.debug("    -> "+sym.fullName)
                }
                if (aam.excludedSymbols.size > 10) {
                  reporter.debug("    -> ...")
                }

                /*
                reporter.debug("  Inlined in ")
                for ((sym, sig) <- aam.inlinedIn.slice(0, 10)) {
                  reporter.debug("    -> "+sym.fullName+" ~> "+sig)
                }
                if (aam.inlinedIn.size > 10) {
                  reporter.debug("    -> ...")
                }
                */
              }
            }

            if (targets.isEmpty) {
              settings.ifDebug {
                reporter.error("no targets found FOR "+aam+": "+aam.obj+" -> "+recSigs+"!")
                debugSymbol(aam.meth)
                reporter.error("Nodes: ")
                for (n <- nodes) {
                  reporter.error(" "+n+": "+n.types)
                }
              }
              //dumpPTE(env, "error.dot")
              //dumpAnalysisStack()

              //reporter.fatal("I will not continue!")
            }

            shouldWeInlineThis(newEnv, aam, targets) match {
              case Left((resolvedTargets, PreciseAnalysis)) => // We should inline this precisely
                var cfg = analysis.cfg

                settings.ifDebug {
                  reporter.debug("Ready to precise-inline for : "+aam+" ("+aam.uniqueID+"). "+resolvedTargets.size+" targets available: "+resolvedTargets.map(_.cfg.symbol.fullName)+" for "+nodes.size+" receivers")

                  resolvedTargets.filterNot(_.cfg.isFlat).foreach { case ResolvedTargetInfo(cfg, sig) =>
                    reporter.debug(" -> Because "+safeFullName(cfg.symbol)+" is not flat!")
                  }
                }

                assert(edge != None, "We ended up precisely analyzing the CFG without an edge information, this can't happen since we need an edge to inline the CFG")

                for (t <- resolvedTargets) {
                  callGraph.addMethodCall(fun.symbol, t.cfg.symbol)
                }
                // 1) Remove current edge
                val rEdge = edge.get

                val nodeA = rEdge.v1
                val nodeB = rEdge.v2


                if (resolvedTargets.size == 0) {
                  // We could not inline anything and there is nothing to
                  // inline anymore, call is impossible
                  env = new PTEnv(BottomEffect)
                } else {
                  // We replace the call node with a call node tracking the inlined targets
                  cfg -= rEdge
                  cfg += new CFGEdge(nodeA, aam.excludeSymbols(resolvedTargets.map(_.cfg.symbol)), nodeB)

                  for (ResolvedTargetInfo(targetCFG, sig) <- resolvedTargets) {

                    var map = Map[CFGTrees.Ref, CFGTrees.Ref]()
                    val typeMap   = sig.tm

                    var connectingEdges = Set[CFG.Statement]()

                    // 2) Build renaming map:
                    //  a) mapping args
                    for ((callArg, funArg) <- aam.args zip targetCFG.args) {
                      callArg match {
                        case r: CFGTrees.Ref =>
                          map += funArg -> r
                        case _ =>
                          // Mapping simple values is not possible, we map by assigning
                          connectingEdges += new CFG.AssignVal(funArg, callArg)
                      }
                    }

                    // b) mapping receiver
                    aam.obj match {
                        case r: CFGTrees.Ref =>
                          map += targetCFG.mainThisRef -> r

                        case nr: CFGTrees.Null =>
                          // Ignore, most likely null == or null.eq

                        case _ =>
                          reporter.error("Unnexpected non-ref for the receiver!", aam.pos)
                    }

                    // c) mapping retval
                    map += targetCFG.retval -> aam.r

                    // 3) Rename targetCFG
                    //reporter.debug("in: "+fun.symbol+": Typemap for inlining "+targetCFG.symbol.fullName+": "+typeMap)
                    //reporter.debug("  => "+nodes.map(n => n+"["+n.types+"]"))
                    //reporter.debug("  MAP: "+map)
                    val renamedCFG = new FunctionCFGInliner(map, typeMap, aam.uniqueID, Some((targetCFG.symbol, sig))).copy(targetCFG)

                    // 4) Connect renamedCFG to the current CFG
                    if (connectingEdges.isEmpty) {
                      // If no arg was explicitely mapped via assigns, we still need to connect to the CFG
                      cfg += (nodeA, CFG.Skip, renamedCFG.entry)
                    } else {
                      for(stmt <- connectingEdges) {
                        cfg += (nodeA, stmt, renamedCFG.entry)
                      }
                    }

                    // 5) Adding CFG Edges
                    for (tEdge <- renamedCFG.graph.E) {
                      cfg += tEdge
                    }

                    // 6) Retval has been mapped via renaming, simply connect it
                    cfg += (renamedCFG.exit, CFG.Skip, nodeB)
                  }

                  settings.ifDebug {
                    reporter.debug("Restarting analysis of "+fun.symbol.fullName+"...")
                  }

                  cfg = cfg.removeSkips.removeIsolatedVertices

                  settings.ifDebug {
                    withDebugCounter { cnt =>
                      dumpCFG(cfg, "restart-"+cnt+".dot");
                    }
                    reporter.debug("Restarting analysis of "+fun.symbol.fullName+"...")
                  }

                  // We need to replace the stackframe with the up to date CFG
                  val frame         = analysisStack.top
                  val newFrame      = new AnalysisContext(cfg, frame.sig, frame.mode)
                  newFrame.tSpent   = frame.timeSpent()

                  analysisStack     = analysisStack.pop.push(newFrame)

                  analysis.restartWithCFG(cfg)
                }

              case Left((resolvedTargets, BluntAnalysis)) if resolvedTargets.isEmpty =>
                // No need to display a warning if it's simply that all available targets have been excluded
                if (aam.excludedSymbols.size < targets.size) {
                  settings.ifDebug {
                    reporter.error(List("For: "+aam,
                      "Assigning to bottom, no available targets. Missing targets:") ::: (targets.map(_.sym) -- aam.excludedSymbols).map(s => " - "+uniqueFunctionName(s)).toList, aam.pos)
                  }
                }

                env = new PTEnv(BottomEffect)

              case Left((resolvedTargets, BluntAnalysis)) => // We should inline this in a blunt fashion

                settings.ifDebug {
                  reporter.debug("Ready to blunt-inline for : "+aam+" ("+aam.uniqueID+"), "+resolvedTargets.size+" targets available: "+resolvedTargets.map(_.cfg.symbol.fullName).mkString(", ")+" ("+targets.size+" requested, "+aam.excludedSymbols.size+" excluded) for "+nodes.size+" receivers")
                }

                var allMappedRets = Set[Node]()

                for (t <- resolvedTargets) {
                  callGraph.addMethodCall(fun.symbol, t.cfg.symbol)
                }

                val envs = resolvedTargets.map { case ResolvedTargetInfo(targetCFG, sig) =>
                  var innerG    = targetCFG.getFlatEffect;
                  val typeMap   = sig.tm

                  var targetArgs        = targetCFG.args
                  var targetMainThisRef = targetCFG.mainThisRef
                  var targetRetval      = targetCFG.retval

                  if (innerG.category.isEmpty) {
                    env
                  } else if (innerG.category.isTop) {
                    innerG
                  } else {
                    //reporter.debug("in: "+fun.symbol+": Typemap for blinlining "+targetCFG.symbol.fullName+": "+typeMap)
                    //reporter.debug("  => "+nodes.map(n => n+"["+n.types+"]"))
                    /**
                     * In an inlining merge graph:
                     *
                     * - First step is to map types.
                     * - Then, we map local variables for args and return
                     * values, once those are mapped correctly, we proceed with
                     * mergeGraph as usual.
                     */
                    if (!typeMap.isEmpty) {
                      //println("#### "+safeFileName(name) +"##########################")

                      //withDebugCounter { cnt =>
                      //  dumpCFG(targetCFG, "cfg-"+cnt+".dot");
                      //  dumpPTE(innerG, "before-"+cnt+".dot");
                      //}

                      val replacer      = new PTEnvReplacer(typeMap, Map());

                      innerG            = replacer.copy(innerG)

                      // Since we may have replaced References in innerG, we
                      // need to replace the CFG refs used later on
                      targetArgs        = targetArgs.map(replacer.copyRef)
                      targetRetval      = replacer.copyRef(targetRetval)
                      targetMainThisRef = replacer.copyThisRef(targetMainThisRef)

                      //withDebugCounter { cnt =>
                      //  dumpPTE(innerG, "after-"+cnt+".dot");
                      //}
                    }

                    var refMap    = Map[CFG.Ref, CFG.SimpleValue]();

                    // 1) Mapping refs:
                    //   a) mapping args
                    refMap ++=  targetArgs zip aam.args

                    //   b) mapping receiver
                    aam.obj match {
                        case r: CFGTrees.Ref =>
                          refMap += targetMainThisRef -> r

                        case nr: CFGTrees.Null =>
                          // Ignore, most likely null == or null.eq

                        case _ =>
                          reporter.error("Unnexpected non-ref for the receiver!", aam.pos)
                    }

                    // 2) We apply the same mapping but on corresponding nodes:
                    var newOuterG = env;
                    var nodeMap   = NodeMap();

                    for ((iRef, oRef) <- refMap) {
                      val (newOG, outerNodes) = newOuterG.getNodes(oRef)

                      val innerNodes = innerG.locState(iRef)

                      newOuterG = newOG

                      for (innerNode <- innerNodes) {
                        nodeMap ++= innerNode -> outerNodes
                      }
                      //for (innerNode <- innerNodes; outerNode <- outerNodes) {
                      //  val (tmpEnv, newOuterNode) = refineNode(newOuterG, innerNode, outerNode);
                      //  newOuterG = tmpEnv
                      //  nodeMap += innerNode -> newOuterNode
                      //}
                    }

                    var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, aam.uniqueID, aam.pos, true)

                    if (!(innerG.locState contains targetRetval)) {
                      withDebugCounter { cnt => 
                        dumpPTE(env,        "before-"+cnt+".dot")
                        dumpPTE(newOuterG2, "after-"+cnt+".dot")
                        dumpPTE(innerG,     "inner-"+cnt+".dot")
                        dumpCFG(targetCFG,  "cfg-"+cnt+".dot")
                      }
                    }

                    val mappedRet = innerG.locState(targetRetval) flatMap newNodeMap

                    if (mappedRet.isEmpty) {
                      settings.ifDebug {
                        reporter.debug("Return values are empty for target "+safeFullName(targetCFG.symbol)+". "+ targetRetval+" points internally to : "+innerG.locState(targetRetval), aam.pos)

                        println(newNodeMap)

                        withDebugCounter { cnt =>
                          dumpPTE(env,        "before-"+cnt+".dot")
                          dumpPTE(newOuterG2, "after-"+cnt+".dot")
                          dumpPTE(innerG,     "inner-"+cnt+".dot")
                          dumpCFG(targetCFG,  "cfg-"+cnt+".dot")
                        }
                      }


                      newOuterG2 = new PTEnv(BottomEffect)
                    } else {
                      // We still need to modify the locstate for the return value
                      newOuterG2 = newOuterG2.setL(aam.r, mappedRet)
                    }

                    settings.ifDebug {
                      withDebugCounter { cnt =>
                        reporter.debug(" --> After handling target: "+targetCFG.symbol.fullName)
                        dumpPTE(env,        "bef-"+cnt+".dot")
                        dumpPTE(innerG,     "inl-"+cnt+".dot")
                        dumpInlining(innerG, newOuterG, newOuterG2, nodeMap.map, newNodeMap.map, "comp-"+cnt+".dot");
                        dumpPTE(newOuterG2, "aft-"+cnt+".dot")
                      }
                    }

                    allMappedRets ++= mappedRet

                    //withDebugCounter { cnt =>
                    //  reporter.debug(" --> After handling target: "+targetCFG.symbol.fullName)
                    //  dumpPTE(env,        "bef-"+cnt+".dot")
                    //  dumpPTE(innerG,     "inl-"+cnt+".dot")
                    //  dumpInlining(innerG, newOuterG, newOuterG2, nodeMap.map, newNodeMap.map, "comp-"+cnt+".dot");
                    //  dumpPTE(newOuterG2, "aft-"+cnt+".dot")
                    //}

                    newOuterG2
                  }
                }

                if (!resolvedTargets.isEmpty && allMappedRets.isEmpty) {
                  settings.ifDebug {
                    if (!resolvedTargets.forall(_.cfg.getFlatEffect.category.isBottom)) {
                      // Only display the error if the effects are not obviously bottom
                      reporter.warn("This method call seem to never return, assigning thus to Bottom!", aam.pos)
                    }
                  }

                  env = new PTEnv(BottomEffect)
                } else {
                  //println("Joining "+envs.size+" envs...")

                  env = PointToLattice.joinUID(aam.uniqueID, envs.toSeq : _*)

                  //withDebugCounter { cnt =>
                  //  dumpPTE(env, "join-"+cnt+".dot")
                  //}

                }

              case Right((reason, isError, continueAsPartial)) =>
                if (isError) {
                  reporter.error(List(
                    "Cannot inline/delay call "+aam+", ignoring call.",
                    "Reason: "+reason), aam.pos)

                } else {
                  settings.ifDebug {

                    reporter.debug(List(
                      "Delaying call to "+aam+"",
                      "Reason: "+reason), aam.pos)

                    reporter.debug("For "+aam.obj+"["+recSigs+"]");
                    for (node <- nodes) {
                      reporter.debug("  "+node+": "+node.types);
                    }
                  }
                }

                if (continueAsPartial) {
                  // From there on, the effects are partial graphs
                  env = env.asPartialEnv(aam, reason)

                  //// Try to recover as much type information as possible. The
                  //// type of all local variable expect for the retval of the
                  //// method call will remian the same

                  //for ((ref, nodes) <- env.locState if ref != aam.r) {
                  //  val sig    = (SigEntry.empty /: nodes) (_ union _.sig)
                  //  val lvnode = LVNode(ref, sig)
                  //  newEnv = newEnv.addNode(lvnode).setL(ref, Set(lvnode))
                  //}

                  //env = newEnv
                } else {
                  env = new PTEnv(BottomEffect)
                }
          }
          case an: CFG.AssignNew => // r = new A
            val iNodeUnique    = INode(an.uniqueID, true,  an.tpe.typeSymbol)
            val iNodeNotUnique = INode(an.uniqueID, false, an.tpe.typeSymbol)

            if (env.ptGraph.V contains iNodeNotUnique) {
              env = env.setL(an.r, Set(iNodeNotUnique))
            } else if (env.ptGraph.V contains iNodeUnique) {
              env = env.replaceNode(iNodeUnique, Set(iNodeNotUnique)).setL(an.r, Set(iNodeNotUnique))
            } else {
              env = env.addNode(iNodeUnique).setL(an.r, Set(iNodeUnique))
            }

          case ac: CFG.AssignCast =>
            val (tmpEnv, nodes) = env.getNodes(ac.rhs)
            var newEnv = tmpEnv

            val castType = if (ac.tpe.typeSymbol.isImplClass) {
              // ac.tpe.typeSymbol.toInterface.tpe
              ac.tpe
            } else {
              ac.tpe
            }

            val newNodes = nodes.flatMap { node =>
              val infoOpt = castType match {
                case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
                  Some(TypeInfo.exact(castType))

                case tpe =>
                  node.types.intersectWith(tpe)
              }

              if (infoOpt.isEmpty) {
                settings.ifDebug {
                  reporter.warn("Impossible cast "+node.types+".asInstanceOf["+castType+"] (cast type was "+ac.tpe+"): "+ac, ac.pos);
                }
                None
              } else {

                val info = infoOpt.getOrElse(TypeInfo.empty)

                //reporter.info("Casting "+node.types+".asInstanceOf["+ac.tpe+"] to "+info)

                val sig = SigEntry.fromTypeInfo(info)

                val newNode = node match {
                  case LVNode(ref, _) =>
                    LVNode(ref, sig)
                  case LNode(fromNode, via, pPoint, _) =>
                    LNode(fromNode, via, pPoint, sig)
                  case n =>
                    n
                }

                if (newNode != node) {
                  newEnv = newEnv.replaceNode(node, Set(newNode))
                }

                Some(newNode)
              }
            }

            if (newNodes.isEmpty) {
              env = BottomPTEnv
            } else {
              env = newEnv.setL(ac.r, newNodes)
            }

          case b: CFG.Branch =>
            b.cond match {
              case i : CFG.IfTrue =>
                val (tmpEnv, nodes) = env.getNodes(i.sv)

                val newNodes: Set[Node] = if (nodes  == Set(BooleanLitNode(false))) {
                  Set()
                } else {
                  nodes
                }

                if (newNodes.isEmpty) {
                  settings.ifDebug {
                    reporter.debug("Impossible branch: "+b)
                  }
                  env = BottomPTEnv
                } else {
                  i.sv match {
                    case r: CFG.Ref =>
                      env = tmpEnv.setL(r, newNodes)
                    case _ =>
                  }
                }

              case i : CFG.IfFalse =>
                val (tmpEnv, nodes) = env.getNodes(i.sv)

                val newNodes: Set[Node] = if (nodes  == Set(BooleanLitNode(true))) {
                  Set()
                } else {
                  nodes
                }

                if (newNodes.isEmpty) {
                  settings.ifDebug {
                    reporter.debug("Impossible branch: "+b)
                  }
                  env = BottomPTEnv
                } else {
                  i.sv match {
                    case r: CFG.Ref =>
                      env = tmpEnv.setL(r, newNodes)
                    case _ =>
                  }
                }

              case i : CFG.IfEqual =>
                val (_, lhsNodes) = env.getNodes(i.lhs)
                val (_, rhsNodes) = env.getNodes(i.rhs)

                if (rhsNodes == Set(NNode) || lhsNodes == Set(NNode)) {
                  // ignore
                } else if (lhsNodes.forall(_.isResolved) && rhsNodes.forall(_.isResolved) && (lhsNodes & rhsNodes).isEmpty) {
                  // Node based equality check
                  settings.ifDebug {
                    reporter.debug("Impossible branch: "+b)
                  }
                  env = BottomPTEnv
                } else {
                  // Type based equality check
                  val lhsTypes = (TypeInfo.empty /:lhsNodes) (_ union _.types)
                  val rhsTypes = (TypeInfo.empty /:rhsNodes) (_ union _.types)

                  if ((lhsTypes intersectWith rhsTypes).isEmpty) {
                    settings.ifDebug {
                      reporter.debug("Impossible branch intersect: "+b)
                    }
                    env = BottomPTEnv
                  }
                }

              case i : CFG.IfNotEqual =>
                val (_, lhsNodes) = env.getNodes(i.lhs)
                val (_, rhsNodes) = env.getNodes(i.rhs)

                // Node based inequality check
                if (lhsNodes.forall(_.isResolved) && rhsNodes.forall(_.isResolved) && lhsNodes == rhsNodes) {
                  settings.ifDebug {
                    reporter.debug("Impossible branch: "+b)
                  }
                  env = BottomPTEnv
                }

              case _ =>

            }

          case bb: CFG.BasicBlock =>
            //reporter.debug("Working with basic block:")
            //dumpPTE(env, "env-before.dot")
            for (stmt <- bb.stmts) {
              env = apply(stmt, env, None)
              //withDebugCounter { cnt => 
              //  reporter.debug("After statement: "+stmt)
              //  dumpPTE(env, "env-after"+cnt+".dot")
              //}
            }

          case _ =>


        }

        env
      }

    }

    def preparePTCFG(fun: AbsFunction, sig: TypeSignature): FunctionCFG = {
        var cfg        = fun.cfg
        var baseEnv    = new PTEnv()

        // 1) We add 'this'/'super'
        val thisNode = LVNode(cfg.mainThisRef, sig.rec)
        baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

        // 2) We add arguments
        for ((a, sigentry) <- cfg.args zip sig.args) {
          val aNode = if (isGroundTypeInfo(sigentry.info)) {
              typeToLitNode(sigentry.info.tpe)
            } else {
              LVNode(a, sigentry)
            }
          baseEnv = baseEnv.addNode(aNode).setL(a, Set(aNode))
        }

        // 3) We add retval
        val retType = TypeInfo.subtypeOf(cfg.retval.tpe)

        val retNode = if (isGroundTypeInfo(retType)) {
          typeToLitNode(cfg.retval.tpe)
        } else {
          LVNode(cfg.retval, SigEntry.fromTypeInfo(retType))
        }

        baseEnv = baseEnv.addNode(retNode).setL(cfg.retval, Set(retNode))

        // 4) If we are in the constructor, we assign all fields defined by this class to their default value
        if (fun.symbol.name == nme.CONSTRUCTOR) {
          for (d <- fun.symbol.owner.tpe.decls if d.isValue && !d.isMethod) {
            val node = typeToLitNode(d.tpe)

            baseEnv = baseEnv.addNode(node).addIEdges(Set(thisNode), Field(d), Set(node))
          }
        }

        // 4) We alter the CFG to put a bootstrapping graph step
        val bstr = cfg.newNamedVertex("bootstrap")

        for (e @ CFGEdge(_, l, v2) <- cfg.graph.outEdges(cfg.entry)) {
          cfg += CFGEdge(bstr, l, v2)
          cfg -= e
        }

        cfg += CFGEdge(cfg.entry, new CFGTrees.Effect(baseEnv, "Bootstrap of "+uniqueFunctionName(fun.symbol)) setTree fun.body, bstr)

        // 5) Finally, we map all types in the typemap
        if (!sig.tm.isEmpty) {
          cfg = new FunctionCFGInliner(Map(), sig.tm, NoUniqueID, None).copy(cfg)

          //dumpCFG(cfg, "prepare-last.dot")
        }


        cfg
    }

    def constructFlatCFG(fun: AbsFunction, completeCFG: FunctionCFG, effect: PTEnv): FunctionCFG = {
        var flatCFG = new FunctionCFG(fun.symbol, completeCFG.args, completeCFG.retval, true)

        //val cnt = getDebugCounter
        var cleanEffect = effect
        //dumpPTE(cleanEffect, "step"+cnt+"-1.dot")
        cleanEffect = cleanEffect.cleanUnreachableForSummary(completeCFG)
        //dumpPTE(cleanEffect, "step"+cnt+"-2.dot")
        cleanEffect = cleanEffect.cleanLocState(completeCFG)
        //dumpPTE(cleanEffect, "step"+cnt+"-3.dot")
        cleanEffect = cleanEffect.cleanExtraLoadEdges()
        //dumpPTE(cleanEffect, "step"+cnt+"-4.dot")
        cleanEffect = cleanEffect.collapseDuplicatedNodes()
        //dumpPTE(cleanEffect, "step"+cnt+"-5.dot")
        cleanEffect = cleanEffect.cleanIsolatedVertices();
        //dumpPTE(cleanEffect, "step"+cnt+"-6.dot")

        flatCFG += (flatCFG.entry, new CFGTrees.Effect(cleanEffect, "Sum: "+uniqueFunctionName(fun.symbol)) setTree fun.body, flatCFG.exit)

        flatCFG
    }

    def analyzePTCFG(fun: AbsFunction, mode: AnalysisMode, sig: TypeSignature): Option[FunctionCFG] = {

      // Prepare CFG for analysis given the sig, if necessary
      val cfg = getPTCFGFromFun(fun, sig)

      if (!analysisStack.isEmpty) {
        analysisStack.top.interrupt()
      }

      // {{{ Global Info

      analysisStackSet += ((fun.symbol, sig))
      analysisStack     = analysisStack.push(new AnalysisContext(cfg, sig, mode))

      reporter.incIndent()

      val oldCache = preciseCallTargetsCache
      preciseCallTargetsCache = Map()

      // }}}

      settings.ifVerbose {
        reporter.msg("Analyzing "+fun.uniqueName+" in "+mode+" with signature "+sig+"...")
      }

      displayAnalysisContext()

      // We run a fix-point on the CFG
      val ttf = new PointToTF(fun, mode)
      val aa = new PTDataFlowAnalysis(PointToLattice, EmptyPTEnv, settings, cfg)

      ttf.analysis = aa

      var oResult: Option[FunctionCFG] = None

      try {
        aa.computeFixpoint(ttf)

        val newCFG     = aa.cfg
        val facts      = aa.getResult
        val exitEffect = facts(cfg.exit)

        // Analysis CFG might have expanded
        settings.ifDebug {
          withDebugCounter { cnt =>
            dumpPTE(exitEffect, "effect-"+cnt+".dot");
          }
        }

        var reducedCFG = if (newCFG.isFlat) {
          newCFG
        } else {
          constructFlatCFG(fun, newCFG, exitEffect)
        }

        val result = if (exitEffect.isPartial) {
          assert(mode != BluntAnalysis, "Obtained non-flat PTCFG while in blunt mode")

          partialReduce(aa, fun, newCFG, facts)
        } else {
          reducedCFG
        }

        settings.ifVerbose {
          reporter.msg("- Done analyzing "+fun.uniqueName)
        }

        oResult = Some(result)
      } catch {
        case aa.AINotMonotoneousException(oldEnv, newEnv, joinedEnv) =>
          joinedEnv diffWith newEnv
          sys.error("Failed to compute fixpoint due to non-monotoneous TF/Lattice!")

        case aue @ AnalysisFallbackException(level, reason) =>
            if (level == 0) {
              settings.ifVerbose {
                reporter.msg("Restarting analysis of "+fun.uniqueName  +" (Reason was: "+reason+")")
              }

              // Parent method will try again
              oResult = None
            } else if (analysisStack.size == 1) {
              // We are at the top level, effect is top
              settings.ifVerbose {
                reporter.warn("Gave up while analyzing "+fun.uniqueName +" (Reason was: "+reason+") (level: "+level+", stack size: "+analysisStack.size+")")
              }

              aue.printStackTrace

              oResult = Some(constructFlatCFG(fun, aa.cfg, TopPTEnv))
            } else {
              throw new AnalysisFallbackException(level-1, reason)
            }
      } finally {

        reporter.decIndent()

        preciseCallTargetsCache = oldCache

        analysisStackSet -= ((fun.symbol, sig))
        analysisStack     = analysisStack.pop

        if (!analysisStack.isEmpty) {
          analysisStack.top.resume()
        }
      }

      if (analysisStack.size > 0) {
        settings.ifDebug {
          oResult match {
            case Some(result) =>
              if (result.isTop) {
                reporter.msg("   Result is Top!");
              } else if (result.isBottom) {
                reporter.msg("   Result is Bottom!");
              } else if (result.isFlat) {
                reporter.msg("   Result is Flat!");
                settings.ifDebug {
                  withDebugCounter { cnt =>
                    dumpPTE(result.getFlatEffect, "result-"+cnt+".dot")
                  }
                }
              } else {
                reporter.msg("   Result is a CFG! Remaining method calls:");
                for (aam <- result.graph.E.collect { case CFGEdge(_, aam: CFGTrees.AssignApplyMeth, _) => aam }) {
                  reporter.msg("    -> "+aam)
                }
                withDebugCounter { cnt =>
                  dumpCFG(result, "result-"+cnt+".dot")
                }
              }
            case None =>
              reporter.debug("Got no result.. will retry")
          }

        }

        reporter.msg("... continuing analyzing "+analysisStack.top.cfg.symbol.fullName)

        settings.ifDebug {
          withDebugCounter { cnt =>
            dumpCFG(analysisStack.top.cfg, "continue-"+cnt+".dot")
          }
        }
      }

      displayAnalysisContext()

      oResult
    }

    def partialReduce(aa: PTDataFlowAnalysis, fun: AbsFunction, cfg: FunctionCFG, res: Map[CFGVertex, PTEnv]): FunctionCFG = {
      // We partially reduce the result
      val unanalyzed = res(cfg.exit).danglingCalls.contains(_)

      reporter.incIndent()

      settings.ifDebug {
        withDebugCounter { cnt =>
          dumpCFG(cfg, "reduce-"+cnt+".dot");
        }

        reporter.info("Reducing CFG with dangling calls: ")
        for ((aam, reason) <- res(cfg.exit).danglingCalls) {
          reporter.info("  "+aam+": "+reason)
        }
      }
      reporter.incIndent()

      var newCFG: FunctionCFG = BasicBlocksBuilder.composeBlocks(cfg, { case e: CFG.AssignApplyMeth => unanalyzed(e) })

      val tf       = new PointToTF(fun, ReductionAnalysis)
      tf.analysis  = aa


      try {
        val cfgCopier = new CFGCopier {
          def copyStmt(stmt: CFG.Statement): CFG.Statement = stmt match {
            case aam: CFG.AssignApplyMeth if (!unanalyzed(aam)) =>
              var env = new PTEnv(EmptyEffect)

              env = tf.apply(aam, env, None)

              env = env.cleanUnreachableForPartial()
              settings.ifDebug {
                if (env.category.isBottom) {
                  reporter.info("Partial reduction ended up in BOTTOM: "+aam)
                } else if (env.category.isTop) {
                  reporter.info("Partial reduction ended up in TOP: "+aam)
                }
              }

              new CFG.Effect(env, "") setInfoFrom aam

            case bb: CFG.BasicBlock =>
              var env = new PTEnv(EmptyEffect)

              for (stmt <- bb.stmts) {
                env = tf.apply(stmt, env, None)
              }

              env = env.cleanUnreachableForPartial()

              settings.ifDebug {
                if (env.category.isBottom) {
                  reporter.info("Partial reduction ended up in BOTTOM: "+bb.stmts)
                } else if (env.category.isTop) {
                  reporter.info("Partial reduction ended up in TOP: "+bb.stmts)
                }
              }

              new CFG.Effect(env, "") setInfoFrom bb

            case other =>
              other
          }
        }


        newCFG = newCFG.copy(graph = cfgCopier.copy(newCFG.graph))

        // We have replaced linear shapes with basic blocks that have been replaced with effects.
        // It remains to check for three scenarios, in order of precedence:
        //    |               |
        //    o     join      o
        //   / \*    ==>      |
        //   \ /*             o
        //    o

        var newGraph = newCFG.graph.mutable
        var changed  = false
        // We compact basic blocks together
        for (v <- newGraph.V) (newGraph.inEdges(v), newGraph.outEdges(v)) match {
          case (ins, outs) if (outs.size >= 2) && outs.forall(_.v2 == outs.head.v2) =>
            if (outs.forall(_.label.isInstanceOf[CFG.Effect])) {
              for (o <- outs) newGraph -= o

              val effects = outs.collect{ case CFGEdge(_, e: CFG.Effect, _) => e.env }.toSeq
              val effect = PointToLattice.join(v, effects: _*)

              val tree =  outs.map(_.label.tree.getOrElse(EmptyTree)).find(_ != EmptyTree).getOrElse(EmptyTree)

              newGraph += CFGEdge(v, (new CFG.Effect(effect, ""): CFG.Statement) setTree tree ,outs.head.v2)
            }

          case _ =>
            // ignore
        }

        for (e <- newGraph.E.collect{ case e @ CFGEdge(_, eff: CFG.Effect, _) if eff.env.category.isBottom => e }) {
          changed = true
          newGraph -= e
        }

        if (changed) {
          newCFG = newCFG.copy(graph = newGraph.immutable)
        }

      } catch {
        case aue: AnalysisFallbackException =>
          reporter.decIndent()
          reporter.decIndent()

         throw aue
      }

      reporter.decIndent()
      settings.ifDebug {
        reporter.info("Done.")
      }
      reporter.decIndent()

      newCFG
    }

    def analyze(fun: AbsFunction) = {
      specializedAnalyze(fun, PreciseAnalysis, TypeSignature.fromDeclaration(fun))
    }

    def specializedAnalyze(fun: AbsFunction, mode: AnalysisMode, sig: TypeSignature) = {
      val tStart = System.currentTimeMillis

      var oResult: Option[FunctionCFG] = None

      while(oResult.isEmpty) {
        oResult = analyzePTCFG(fun, mode, sig)
      }

      val result = oResult.get

      if (mode == PreciseAnalysis) {
        // We only record precise analyses here in the "official" PTCFG store
        fun.ptCFGs += sig -> (result, true)
      }

      if (result.isFlat) {
        fun.flatPTCFGs     += sig -> result
        fun.flatPTCFGsTime += sig -> (fun.flatPTCFGsTime(sig)+(System.currentTimeMillis - tStart))
      }

      result
    }

    // WARNING: This is only used when we have a global callgraph, i.e. NEVER
    def analyzeSCC(scc: Set[Symbol]) {
      // The analysis is only run on symbols that are actually AbsFunctions, not all method symbols

      var workList = scc

      // 1) First, we remove from the worklist functions that we cannot analyze
      for (sym <- scc if !(declaredFunctions contains sym)) {
        workList -= sym
      }

      // 2) Then, we analyze every methods until we reach a fixpoint
      while(!workList.isEmpty) {
        val sym = workList.head
        workList = workList.tail

        ptProgressBar.draw()

        if (declaredFunctions contains sym) {
          val fun = lookupFunction(sym).get

          val cfgBefore  = getPTCFGFromFun(fun)

          globalTStart = System.currentTimeMillis

          analyze(fun)

          val cfgAfter   = getPTCFGFromFun(fun)

          if (cfgBefore != cfgAfter && cfgAfter.isTop) {
            workList ++= (simpleReverseCallGraph(sym) & scc)
          }
        }
      }
    }

    /*
    def fillDatabase() {
      if (Database.active) {
        reporter.msg("Inserting "+declaredFunctions.size+" graph entries in the database...")

        val toInsert = for ((s, fun) <- declaredFunctions) yield {

          val (name, e, isSynth) = getResultEnv(fun)

          (name, new EnvSerializer(e).serialize(), isSynth)
        }

        Database.Env.insertAll(toInsert)
      } else {
        reporter.error("Cannot insert into database: No database configuration")
      }
    }
    */

    /*
    def fillPartial(fun: AbsFunction) {
      if (Database.active) {
        val (name, e, isSynth) = getResultEnv(fun)

        val toInsert = List((name, new EnvSerializer(e).serialize(), isSynth))

        Database.Env.insertAll(toInsert)
      } else {
        reporter.error("Cannot insert into database: No database configuration")
      }
    }
    */

    lazy val ptProgressBar = settings.getAnalysisProgressBar()

    def run() {

      if (settings.onDemandMode) {
        reporter.msg("Computing worklist...")

        var workList: List[AbsFunction] = declaredFunctions.values.filter(fun => settings.onDemandFunction(safeFullName(fun.symbol))).toList.sortBy(_.symbol.fullName)

        var rangeStart = 0
        var rangeEnd   = workList.size

        if (settings.wlSkipFirst > 0) {
          workList = workList.drop(settings.wlSkipFirst)
          rangeStart = settings.wlSkipFirst
        }

        if (settings.wlStopAfter > 0) {
          workList = workList.take(settings.wlStopAfter)
          rangeEnd = settings.wlSkipFirst + settings.wlStopAfter
        }

        reporter.msg("Demand driven analysis of "+workList.size+" functions ("+rangeStart+" -> "+rangeEnd+")")

        ptProgressBar.setMax(workList.size)
        ptProgressBar.draw()

        while(!workList.isEmpty) {
          val fun = workList.head
          workList = workList.tail

          val cfgBefore  = getPTCFGFromFun(fun)
          globalTStart = System.currentTimeMillis
          analyze(fun)
          val cfgAfter   = getPTCFGFromFun(fun)

          if (cfgAfter != cfgBefore && !cfgAfter.isFlat) {
            // we only reanalyze if the new cfg changed and is non trivial
            workList = fun :: workList
          } else {
            // Finished analyzing fun
            resultsStats.total += 1

            val category = if (cfgAfter.isFlat) {
              if (cfgAfter.isTop) {
                resultsStats.top +=1
                "top"
              } else if (cfgAfter.isBottom) {
                resultsStats.bot +=1
                "bottom"
              } else if (cfgAfter.isPure) {
                resultsStats.pure +=1
                "pure"
              } else {
                resultsStats.impure +=1
                "impure"
              }
            } else {

              val condCFG = analyzePTCFG(fun, ConditionalAnalysis, TypeSignature.fromDeclaration(fun.symbol))

              condCFG match {
                case Some(cfg) if cfg.isPure =>
                  resultsStats.condPure +=1
                  "condPure"

                case Some(cfg) =>
                  resultsStats.condImpure +=1
                  "condImpure"

                case None =>
                  reporter.error("Failed to produce an effect, BLEH")
                  resultsStats.condImpure +=1
                  "ERROR"
              }
            }

            lastResults.append((fun.symbol, category))
            resultsLog.println(category+"\t"+fun.symbol.fullName)

            ptProgressBar.tick
            ptProgressBar.draw()
          }
        }

        ptProgressBar.end();

      } else {
        // 1) Analyze each SCC in sequence, in the reverse order of their topological order
        //    We first analyze {M,..}, and then methods that calls {M,...}
        var workList = callGraphSCCs.reverse.map(scc => scc.vertices.map(v => v.symbol))

        val totJob   = workList.map(_.size).sum

        ptProgressBar.setMax(totJob)
        ptProgressBar.draw()

        for (scc <- workList) {
          analyzeSCC(scc)
          ptProgressBar ticks scc.size
          ptProgressBar.draw()
        }

        ptProgressBar.end();
      }

      // 2) Fill graphs in the DB, if asked to
      //if (settings.fillGraphs && !settings.fillGraphsIteratively) {
      //  fillDatabase()
      //}

      if (settings.dumpCallGraph) {
        val path = "callgraph.dot"
        reporter.msg("Dumping Call Graph to "+path)
        new DotConverter(callGraph, "Call Graph Analysis").writeFile(path)
      }

      // 4) Display/dump results, if asked to
      if (!settings.dumpptgraphs.isEmpty) {

        var toDump = allFunctions.values.collect { case f: AbsFunction if settings.dumpPTGraph(safeFullName(f.symbol)) => (f.symbol, f) }

        reporter.msg(" Summary of generated effect-graphs:")

        val columns = Seq(TableColumn("Function Name", Some(80)),
                          TableColumn("ID", None),
                          TableColumn("Signature", Some(80)),
                          TableColumn("Type", None),
                          TableColumn("DC", None),
                          TableColumn("#E", None),
                          TableColumn("ms", None)
                          )

        var cntFun = 0
        var cntSig = 0
        val table = new Table(columns)

        for ((s, fun) <- toDump.toSeq.sortBy(x => safeFullName(x._1))) {
          var i = 0;
          val name = uniqueFunctionName(fun.symbol)

          val preciseCFGs = fun.ptCFGs.filter { case (_, (cfg, isAnalyzed)) => !cfg.isFlat && isAnalyzed }
          for((sig, (res, _)) <- preciseCFGs) {
            val callsRemaining = res.graph.E.filter(_.label.isInstanceOf[CFG.AssignApplyMeth]).size

            table.addRow(TableRow() | fun.symbol.fullName | i.toString | sig.toString | "precise" | callsRemaining.toString | "?" | "?" )

            val dest = safeFileName(name)+"-"+i+"-ptcfg.dot"
            new CFGDotConverter(res, "").writeFile(dest)
            i += 1
            cntSig += 1
          }

          for((sig, res) <- fun.flatPTCFGs) {
            val effect = res.getFlatEffect
            val effectType = effect.category match {
              case BottomEffect => "bottom"
              case TopEffect => "top"
              case _  => "flat"
            }
            val nIEdges = effect.iEdges.size + effect.oEdges.size
            val time = fun.flatPTCFGsTime.getOrElse(sig, "?").toString

            table.addRow(TableRow() | fun.symbol.fullName | i.toString | sig.toString | effectType | "-" | nIEdges.toString | time )
            val dest = safeFileName(name)+"-"+i+"-ptcfg.dot"
            new PTDotConverter(effect, "").writeFile(dest)
            i += 1
            cntSig += 1
          }
          if (i > 0) {
            cntFun += 1
          }
        }

        reporter.dispatch(table.draw _)
        reporter.msg(" -> Analyzed "+cntSig+" signatures of "+cntFun+" functions")
      }

      if (!settings.displaypure.isEmpty) {

        val columns = Seq(TableColumn("Function Name", Some(80)),
                          TableColumn("Signature", Some(40)),
                          TableColumn("Effect", Some(200)))

        val table = new Table(columns)

        var toDump = allFunctions.values.collect { case f: AbsFunction if settings.displayPure(safeFullName(f.symbol)) => (f.symbol, f) }

        for ((s, fun) <- toDump.toSeq.sortBy(x => safeFullName(x._1))) {
          if (!fun.flatPTCFGs.isEmpty) {
            for(((sig, res), i) <- fun.flatPTCFGs.zipWithIndex) {
              val effect = res.getFlatEffect

              val effStr = if (effect.category.isBottom) {
                "\u22A5"
              } else if (effect.category.isTop) {
                "\u22A4"
              } else {
                val reg = new RegexEffectRepresentation(effect).getStringRegex
                reg match {
                  case _: RegularExpressions.RegEps[_] =>
                    "\u2118"
                  case _ =>
                    reg.toString
                }
              }

              table.addRow(TableRow() | fun.symbol.fullName | sig.toString | effStr)
            }
          } else {
            val preciseCFGs = fun.ptCFGs.filter { case (_, (cfg, isAnalyzed)) => !cfg.isFlat && isAnalyzed }

            for((sig, (res, _)) <- preciseCFGs) {
              val callsRemaining = res.graph.E.collect { case CFGEdge(_, aam: CFG.AssignApplyMeth, _) => aam.meth.fullName.split("\\.").takeRight(2).mkString(".")+"()" }
              table.addRow(TableRow() | fun.symbol.fullName | sig.toString | "Calls "+callsRemaining.mkString(", ") )
            }
          }
        }

        reporter.title("Effects Results:")
        if (!table.rows.isEmpty) {
          reporter.dispatch(table.draw _)
        } else {
          reporter.title(" -> No effect requested")
        }
      }
    }
  }
}

