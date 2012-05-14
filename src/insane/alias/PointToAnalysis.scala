package insane
package alias

//import storage.Database

import utils._
import utils.Reporters._
import GlobalCounter.withDebugCounter
import utils.Graphs.DotConverter
import CFG._

import scala.tools.nsc.symtab.Flags

trait PointToAnalysis extends PointToGraphsDefs with PointToEnvs with PointToLattices {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  object PTAnalysisModes extends Enumeration {
    val PreciseAnalysis     = Value("PreciseAnalysis")
    val BluntAnalysis       = Value("BluntAnalysis")
    val ReductionAnalysis   = Value("ReductionAnalysis")
  }

  type AnalysisMode = PTAnalysisModes.Value
  import PTAnalysisModes._


  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    type PTDataFlowAnalysis = dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG]

    var predefinedHighPriorityCFG = Map[Symbol, Option[FunctionCFG]]()
    def getPredefHighPriorityCFG(sym: Symbol) = {

      val AllScalaStubs = "^^scala.sys.error.+|^java\\.lang\\.Object\\..+|scala\\.math\\.ScalaNumber\\..+|java\\..+Exception.+".r

      predefinedHighPriorityCFG.get(sym) match {
        case Some(optcfg) =>
          optcfg

        case None =>
          val optcfg = uniqueFunctionName(sym) match {

            case name if settings.consideredPure(name) =>
              Some(buildPureEffect(sym))

            case AllScalaStubs() =>
              Some(buildPureEffect(sym))

            case _ if definitions.isScalaValueType(sym.owner.tpe) =>
              Some(buildPureEffect(sym))

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
          funDecls.get(sym) match {
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
          funDecls.get(sym) match {
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
          funDecls.get(sym) match {
            case Some(fun) =>
              fun.flatPTCFGs.get(sig) match {
                case Some(flatPTCFG) =>
                  // Already here? Nice.
                  Some(flatPTCFG)
                case _ =>
                  // We need to re-analyze in blunt-mode
                  var changed = false;

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

                  var (oldCFG, oldEffect) = computeFlatEffect()

                  do {
                    var (newCFG, newEffect) = computeFlatEffect()
                    //val joinEffect = PointToLattice.join(oldEffect, newEffect)

                    pass += 1;

                    changed = (newEffect != oldEffect)
                    if (pass > 20 && changed) {
                      //reporter.debug(" Before : =================================")
                      //reporter.debug(oldEffect.toString)
                      //reporter.debug(" After  : =================================")
                      //reporter.debug(newEffect.toString)
                      reporter.debug(" Diff   : =================================")
                      oldEffect diffWith newEffect
                    }
                    oldCFG    = newCFG;
                    oldEffect = newEffect;
                  } while(changed);

                  fun.flatPTCFGs += sig -> oldCFG
                  fun.flatPTCFGsTime += sig -> (fun.flatPTCFGsTime(sig) + (System.currentTimeMillis - tStart))

                  reporter.msg("======= Analyzing "+sym.fullName+" required "+pass+" passes!")
                  Some(oldCFG)
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


    class PointToTF(fun: AbsFunction, analysisMode: AnalysisMode) extends dataflow.TransferFunctionAbs[PTEnv, CFG.Statement] {

      var analysis: PTDataFlowAnalysis = null


      def isRecursive(symbol: Symbol) = {
        if (settings.onDemandMode) {
          if (recursiveMethods contains symbol) {
            true
          } else if (analysisStackSet contains symbol) {
            recursiveMethods += symbol
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
      def shouldUseFlatInlining(aam: CFG.AssignApplyMeth, target: Symbol): Boolean = {
        if (isRecursive(target)) {
          if (settings.onDemandFunction(safeFullName(fun.symbol))) {
            true
          } else {
            false
          }
        } else {
          false
        }
      }

      abstract class InlineStrategy {
        def shouldDelay(targetsToConsider: Set[(Symbol, ClassTypeMap)],
                        targetPoolSize: Int,
                        currentFun: AbsFunction): Option[String];
      }

      object InlineStrategies {
        object Smart extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[(Symbol, ClassTypeMap)],
                          targetPoolSize: Int,
                          currentFun: AbsFunction): Option[String] = {

            val score = targetsToConsider.size*2 + targetPoolSize

            if (settings.onDemandFunction(safeFullName(currentFun.symbol))) {
              None // No delaying
            } else if (score > settings.maxInlinableScore) {
              Some("too many targets: "+score+" > "+settings.maxInlinableScore+" ("+targetsToConsider.size+" targets, "+targetPoolSize+" poolsize)")
            } else {
              None // No delaying
            }
          }
        }

        object AlwaysInline extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[(Symbol, ClassTypeMap)],
                          targetPoolSize: Int,
                          currentFun: AbsFunction): Option[String] = {

            None // No delaying
          }
        }

        object AlwaysDelay extends InlineStrategy {
          def shouldDelay(targetsToConsider: Set[(Symbol, ClassTypeMap)],
                          targetPoolSize: Int,
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

      def shouldWeInlineThis(aam: CFG.AssignApplyMeth,
                             sig: TypeSignature,
                             targets: Set[(Symbol, ClassTypeMap)],
                             allTypes: Set[Type]): Either[(Set[(FunctionCFG, DualTypeMap)], AnalysisMode), (String, Boolean, Boolean)] = {

        val symbol            = aam.meth
        val excludedTargets   = aam.excludedSymbols
        val targetsToConsider = targets.filter(t => !excludedTargets(t._1))

        val targetPoolSize    = allTypes.size-excludedTargets.size

        analysisMode match {
          case PreciseAnalysis =>
            if (targets.isEmpty) {
              Right("no target could be found", true, true)
            } else {
              val receiverTypes = sig.rec.info

              if (!receiverTypes.isExhaustive && !settings.assumeClosedWorld) {
                Right("unbouded number of targets", false, true)
              } else {
                val strategy = getInlineStrategy
                val optDelayReason = strategy.shouldDelay(targetsToConsider, targetPoolSize, fun)

                if (!optDelayReason.isEmpty) {
                  Right(optDelayReason.get, false, true)
                } else {
                  var targetsRecursive = false
                  var targetsArbitrary = false
                  var missingTargets = Set[Symbol]()

                  val availableTargets = targetsToConsider flatMap { case (sym, classTypeMap) =>
                    val dualTypeMap = DualTypeMap(classTypeMap, computeMethodTypeMap(sym, aam.typeArgs))

                    val sigPrecise   = sig.copy(tm = dualTypeMap).clampAccordingTo(sym)

                    val optCFG = if (shouldUseFlatInlining(aam, sym)) {
                      // If we use flat inlining, we use the most precise type sig as possible
                      getFlatPTCFG(sym, sigPrecise)
                    } else if (isRecursive(sym)) {
                      // In case we can't use flat inlining, we prevent
                      // inlining in case it is a recursive call.
                      targetsRecursive = true
                      None
                    } else {
                      if (settings.contSenWhenPrecise) {
                        getPTCFGAnalyzed(sym, sigPrecise)
                      } else {
                        getPTCFGAnalyzed(sym, TypeSignature.fromDeclaration(sym))
                      }
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
                        Some((cfg, dualTypeMap))
                    }
                  }

                  if (targetsRecursive) {
                    Right("Recursive calls should stay as-is in precise mode", false, true)
                  } else if (targetsArbitrary) {
                    Right("Some targets are to be considered arbitrarily", false, true)
                  } else if (!missingTargets.isEmpty) {
                    for (mt <- missingTargets) {
                      val fn = mt.fullName

                      println("*** "+uniqueFunctionName(mt))
                      funDecls.filter(_._1.fullName == fn) foreach { fd =>
                        println(" ))) Found similar function "+uniqueFunctionName(fd._1))
                      }
                    }
                    Right("some targets are unanalyzable: "+missingTargets.map(uniqueFunctionName(_)).mkString(", "), true, true)
                  } else {

                    preciseCallTargetsCache += aam -> availableTargets

                    Left((availableTargets, if (availableTargets forall (_._1.isFlat)) BluntAnalysis else PreciseAnalysis))
                  }
                }
              }
            }
          case BluntAnalysis =>
            // We have to analyze this, and thus inline, no choice
            // here, we require that the result is a flat effect
            var missingTargets = Set[Symbol]()

            val targetsCFGs = targetsToConsider flatMap { case (sym, classTypeMap) =>
              val dualTypeMap = DualTypeMap(classTypeMap, computeMethodTypeMap(sym, aam.typeArgs))

              val sigPrecise = sig.copy(tm = dualTypeMap).clampAccordingTo(sym)

              if (settings.consideredArbitrary(safeFullName(sym))) {
                  missingTargets += sym
                  None
              } else {
                getFlatPTCFG(sym, sigPrecise) match {
                  case None =>
                    missingTargets += sym
                    None
                  case Some(cfg)=>
                    Some(cfg, dualTypeMap)
                 }
              }
            }

            preciseCallTargetsCache += aam -> targetsCFGs

            if (targetsToConsider.isEmpty) {
              Left((Set(), BluntAnalysis))
            } else if (targetsCFGs.isEmpty) {
              Right(("Some targets are missing: "+missingTargets.map(uniqueFunctionName(_)).mkString(", "), targetsCFGs.isEmpty, false))
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
        if (oldEnv.isBottom) {
          return oldEnv
        }

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
          if (outerG.isEmpty) {
            innerG
          } else if(innerG.isEmpty) {
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

              for (outerNode <- outerNodes) {
                // Refine the node in case inner <:< outer
                val (tmpEnv, newOuterNode) = refineNode(newOuterG, innerNode, outerNode);
                newOuterG = tmpEnv
                nodeMap += (innerNode -> newOuterNode)

                if (innerNode.types isStrictlyMorePreciseThan newOuterNode.types) {
                  reporter.warn(" AAAAAAAAA "+innerNode+" --> "+newOuterNode)
                }
              }
            }

            var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, uniqueID, pos, allowStrongUpdates)

            for ((r, nodes) <- innerG.locState) {
              val outerNodes = nodes flatMap newNodeMap
              if (outerNodes.isEmpty) {
                reporter.warn("Unable to map local variable "+r+" in the outer environment")
              } else {
                newOuterG2 = newOuterG2.setL(r, outerNodes)
              }
            }

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
          def resolveLoadNode(lNode: LNode, stack: Set[LNode] = Set()): Set[Node] = {
            val LNode(_, field, pPoint, sig) = lNode

            val innerFromNodes = innerG.ptGraph.ins(lNode).collect{ case OEdge(f, _, _) => f }

            val fromNodes = innerFromNodes map ( n => (n, n match {
              case from : LNode if !stack(from) =>
                resolveLoadNode(from, stack + lNode)
              case from =>
                nodeMap(from)
            }))

            var pointedResults = Set[Node]()

            for ((innerFromNode, tmpNodes) <- fromNodes; tmpNode <- tmpNodes) {

              val (tmpOuterG, node) = refineNode(newOuterG, innerFromNode, tmpNode)

              if (node != tmpNode) {
                // Node was refined, we need to affect
                nodeMap += innerFromNode -> node
              }

              newOuterG = tmpOuterG

              //val writeTargets = newOuterG.getWriteTargets(Set(node), field)

              //var pointed = if (writeTargets.isEmpty) {
              //  newOuterG.getReadTargets(Set(node), field)
              //} else {
              //  writeTargets
              //}

              var pointed = newOuterG.getAllTargets(Set(node), field)

              var shouldCreate = true;
              // Filter only compatible point results:
              pointed = pointed.filter { p => p match {
                  case ln: LNode =>
                    if (ln.types == lNode.types) {
                      // We found an exact match, no need to create a LNode
                      shouldCreate = false;
                    }
                    ln.types isMorePreciseThan lNode.types
                  case lv : LVNode =>
                    lv.types isMorePreciseThan lNode.types
                  case _ =>
                    shouldCreate = false;
                    true
                }
              }

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

                    val newLNode = safeTypedLNode(lNode.sig, node, field, newId)

                    //for (nodeToAdd <- findSimilarLNodes(newLNode, newOuterG.ptGraph.V)) {
                    //  newOuterG = newOuterG.addNode(nodeToAdd).addOEdge(node, field, nodeToAdd)
                    //  pointedResults += nodeToAdd
                    //}
                    newOuterG = newOuterG.addNode(newLNode).addOEdge(node, field, newLNode)
                    pointedResults ++= (pointed + newLNode)
                }
              } else {
                pointedResults ++= pointed
              }
            }

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


          do {
            oldOuterG  = newOuterG
            oldNodeMap = nodeMap

            for (oe @ OEdge(v1, lab, v2) <- innerG.oEdges) {
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

              (ov1, ov2) match {
                case (v1s, v2s) if !v1s.isEmpty && !v2s.isEmpty =>
                  newOuterG = newOuterG.addOEdges(v1s, lab, v2s)
                case _ =>
                  // This will occur in case one of the nodes were resolved to a INode
                  // => We wait, once the IEdges are merged in, the OEdges should
                  // find valid sources and targets.
              }
            }

            newOuterG = applyInnerEdgesFixPoint(innerG, newOuterG, nodeMap)

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

            // Helper function to traverse graph and compute type signature from nodes
            def typeSignatureFromNodes(nodes: Iterable[Node], depth: Int): SigEntry = {
              val info = (TypeInfo.empty /: nodes) (_ union _.types)
              val sig  = (SigEntry.empty /: nodes) (_ union _.sig)

              var foundOne = false;

              if (depth == 0) {
                  sig
              } else {
                val fieldsSig = for (sym <- info.tpe.decls if !sym.isMethod) yield {
                  val field = Field(sym)

                  var abort = false;
                  val allTargets = for (n <- nodes if !abort) yield {
                    val targets = newEnv.getWriteOrElseReadTargets(Set(n), field);

                    if (targets.isEmpty) {
                      abort = true;
                    }
                    targets
                  }

                  val targets = allTargets.flatten

                  if (abort || targets.isEmpty) {
                    // Some targets may have been found, but not for all nodes
                    val tpe = TypeInfo.subtypeOf(info.tpe.memberType(sym))

                    val fieldInfo = (tpe /: targets) (_ union _.types)
                    val fieldSig  = (SigEntry.fromTypeInfo(tpe) /: targets) (_ union _.sig)

                    if ((fieldSig != SigEntry.fromTypeInfo(fieldInfo)) || (fieldInfo != tpe)) {
                      foundOne = true;
                    }

                    field -> fieldSig
                  } else {
                    foundOne = true;
                    field -> typeSignatureFromNodes(allTargets.flatten, depth-1)
                  }
                }

                if (foundOne) {
                  // We have at least one field that is precise
                  FieldsSigEntry(info, fieldsSig.toMap)
                } else {
                  sig
                }
              }
            }

            val objTpe = (TypeInfo.empty /: nodes) (_ union _.types)

            val (info, recSig) = aam.obj match {
              case CFG.SuperRef(sym, _, tpe) =>
                val res = TypeInfo.exact(tpe.asSeenFrom(objTpe.tpe, tpe.typeSymbol))
                (res, SigEntry.fromTypeInfo(res))
              case _ =>
                (objTpe, typeSignatureFromNodes(nodes, settings.contSenDepth))
            }

            val callArgsSigs = for (a <- aam.args) yield {
              val (tmp, nodes) = newEnv.getNodes(a)
              newEnv = tmp
              typeSignatureFromNodes(nodes, settings.contSenDepth)
            }

            val callSig = TypeSignature(recSig, callArgsSigs, DualTypeMap.empty)

            val allReceiverTypes = info.resolveTypes

            val typeMap = computeTypeMap(aam.meth, aam.typeArgs, info)

            val methodType = typeMap(aam.meth.tpe)

            settings.ifDebug {
              reporter.debug("Currently handling: "+aam)
              reporter.debug("  Sig:      "+callSig)
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
              reporter.debug("  Receiver: "+aam.obj+": (nodes: "+nodes+") "+info)
              reporter.debug("  Analysis Mode: "+analysisMode)
            }

            if (nodes.isEmpty) {
              dumpAnalysisStack()
              reporter.fatalError("IMPOSSIBRU! Could not find any node for the receiver of: "+aam)
            }

            var targets = getMatchingMethods(aam.meth, methodType, info)

            settings.ifDebug {
              reporter.debug("  Targets("+targets.size+") :")
              for ((sym, map) <- targets.slice(0, 10)) {
                reporter.debug("  -> "+sym.fullName)
              }
              if (targets.size > 10) {
                reporter.debug("  -> ...")
              }
            }

            if (targets.isEmpty) {
              targets = getPredefHighPriorityCFG(aam.meth) match {
                case Some(x) =>
                  // It will be pure or predefined anyway!
                  Set((aam.meth, ClassTypeMap(Map())))
                case _ =>
                  Set()
              }
            }

            //if (targets.isEmpty) {
            //  reporter.error("no targets found FOR "+aam+": "+aam.obj+" -> "+info+"!")
            //  debugSymbol(aam.meth)
            //  reporter.error("Nodes: ")
            //  for (n <- nodes) {
            //    reporter.error(" "+n+": "+n.types)
            //  }

            //  dumpPTE(env, "error.dot")
            //  println(info.tpe.decls)

            //  dumpAnalysisStack()

            //  reporter.fatalError("I will not continue!")
            //}

            shouldWeInlineThis(aam, callSig, targets, allReceiverTypes) match {
              case Left((targetCFGs, PreciseAnalysis)) => // We should inline this precisely
                var cfg = analysis.cfg

                settings.ifDebug {
                  reporter.debug("Ready to precise-inline for : "+aam+". "+targetCFGs.size+" targets available: "+targetCFGs.map(_._1.symbol.fullName)+" for "+nodes.size+" receivers")

                  targetCFGs.filterNot(_._1.isFlat).foreach { case (cfg, map) =>
                    reporter.debug(" -> Because "+safeFullName(cfg.symbol)+" is not flat!")
                  }
                }

                assert(edge != None, "We ended up precisely analyzing the CFG without an edge information, this can't happen since we need an edge to inline the CFG")

                // 1) Remove current edge
                val rEdge = edge.get

                val nodeA = rEdge.v1
                val nodeB = rEdge.v2


                if (targetCFGs.size == 0) {
                  // We could not inline anything and there is nothing to
                  // inline anymore, call is impossible
                  env = new PTEnv(isBottom = true)
                } else {
                  // We replace the call node with a call node tracking the inlined targets
                  cfg -= rEdge
                  cfg += new CFGEdge(nodeA, aam.excludeSymbols(targetCFGs.map(_._1.symbol)), nodeB)

                  for ((targetCFG, typeMap) <- targetCFGs) {

                    var map = Map[CFGTrees.Ref, CFGTrees.Ref]()

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
                        case _ =>
                          reporter.error("Unnexpected non-ref for the receiver!", aam.pos)
                    }

                    // c) mapping retval
                    map += targetCFG.retval -> aam.r

                    // 3) Rename targetCFG
                    //reporter.debug("in: "+fun.symbol+": Typemap for inlining "+targetCFG.symbol.fullName+": "+typeMap)
                    //reporter.debug("  => "+nodes.map(n => n+"["+n.types+"]"))
                    //reporter.debug("  MAP: "+map)
                    val renamedCFG = new FunctionCFGInliner(map, typeMap, aam.uniqueID).copy(targetCFG)

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

                  // We need to replace the stackframe with the up to date CFG
                  val frame = analysisStack.head
                  analysisStack     = analysisStack.pop.push(new AnalysisContext(cfg, frame.sig, frame.mode))

                  analysis.restartWithCFG(cfg)
                }

              case Left((targetCFGs, BluntAnalysis)) if targetCFGs.isEmpty =>
                // No need to display a warning if it's simply that all available targets have been excluded
                if (aam.excludedSymbols.size < targets.size) {
                  settings.ifDebug {
                    reporter.error(List("For: "+aam,
                                        "No available targets while in blunt mode, assigning to bottom!"), aam.pos)
                  }
                }

                env = new PTEnv(isBottom = true)

              case Left((targetCFGs, BluntAnalysis)) => // We should inline this in a blunt fashion

                settings.ifDebug {
                  reporter.debug("Ready to blunt-inline for : "+aam+", "+targetCFGs.size+" targets available: "+targetCFGs.map(_._1.symbol.fullName).mkString(", ")+" ("+targets.size+" requested, "+aam.excludedSymbols.size+" excluded) for "+nodes.size+" receivers")
                }

                var allMappedRets = Set[Node]()

                val envs = targetCFGs.map { case (targetCFG, typeMap) =>
                  var innerG    = targetCFG.getFlatEffect;

                  var targetArgs        = targetCFG.args
                  var targetMainThisRef = targetCFG.mainThisRef
                  var targetRetval      = targetCFG.retval

                  if (innerG.isEmpty) {
                    env
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
                      // println("#### "+safeFileName(name) +"##########################")

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

                      for (innerNode <- innerNodes; outerNode <- outerNodes) {
                        val (tmpEnv, newOuterNode) = refineNode(newOuterG, innerNode, outerNode);
                        newOuterG = tmpEnv
                        nodeMap += innerNode -> newOuterNode
                      }
                    }

                    var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, aam.uniqueID, aam.pos, true)

                    val mappedRet = innerG.locState(targetRetval) flatMap newNodeMap

                    if (mappedRet.isEmpty) {
                      settings.ifDebug {
                        reporter.debug("Return values are empty for target "+safeFullName(targetCFG.symbol)+". "+ targetRetval+" points internally to : "+innerG.locState(targetRetval), aam.pos)

                        withDebugCounter { cnt =>
                          dumpPTE(env,        "before-"+cnt+".dot")
                          dumpPTE(newOuterG2, "after-"+cnt+".dot")
                          dumpPTE(innerG,     "inner-"+cnt+".dot")
                          dumpCFG(targetCFG,  "cfg-"+cnt+".dot")
                        }
                      }


                      //withDebugCounter { cnt =>
                      //  reporter.debug(" --> After handling target: "+targetCFG.symbol.fullName)
                      //  dumpPTE(env,        "bef-"+cnt+".dot")
                      //  dumpPTE(innerG,     "inl-"+cnt+".dot")
                      //  dumpInlining(innerG, newOuterG, newOuterG2, nodeMap.map, newNodeMap.map, "comp-"+cnt+".dot");
                      //  dumpPTE(newOuterG2, "aft-"+cnt+".dot")
                      //}
                      newOuterG2 = new PTEnv(isBottom = true)
                    } else {
                      // We still need to modify the locstate for the return value
                      newOuterG2 = newOuterG2.setL(aam.r, mappedRet)
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

                if (!targetCFGs.isEmpty && allMappedRets.isEmpty) {
                  settings.ifDebug {
                    if (!targetCFGs.forall(_._1.getFlatEffect.isBottom)) {
                      // Only display the error if the effects are not obviously bottom
                      reporter.warn("This method call seem to never return, assigning thus to Bottom!", aam.pos)
                    }
                  }

                  env = new PTEnv(isBottom = true)
                } else {
                  //println("Joining "+envs.size+" envs...")

                  env = PointToLattice.join(envs.toSeq : _*)

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

                    reporter.debug("For "+aam.obj+"["+info+"]");
                    for (node <- nodes) {
                      reporter.debug("  "+node+": "+node.types);
                    }
                  }
                }

                if (continueAsPartial) {
                  // From there on, the effects are partial graphs
                  var newEnv = env.asPartialEnv(env.danglingCalls + (aam -> reason))

                  // Try to recover as much type information as possible. The
                  // type of all local variable expect for the retval of the
                  // method call will remian the same

                  for ((ref, nodes) <- env.locState if ref != aam.r) {
                    val sig    = (SigEntry.empty /: nodes) (_ union _.sig)
                    val lvnode = LVNode(ref, sig)
                    newEnv = newEnv.addNode(lvnode).setL(ref, Set(lvnode))
                  }

                  env = newEnv
                } else {
                  env = new PTEnv(isBottom = true)
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

            var isIncompatible = false

            val castType = if (ac.tpe.typeSymbol.isImplClass) {
              // ac.tpe.typeSymbol.toInterface.tpe
              ac.tpe
            } else {
              ac.tpe
            }



            val newNodes = for (node <- nodes) yield {
              val infoOpt = castType match {
                case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
                  Some(TypeInfo.exact(castType))

                case tpe =>
                  node.types.intersectWith(tpe)
              }

              if (infoOpt.isEmpty) {
                settings.ifDebug {
                  reporter.error("Impossible cast "+node.types+".asInstanceOf["+castType+"] (cast type was "+ac.tpe+")", ac.pos);
                }
                isIncompatible = true
              }

              val info = infoOpt.getOrElse(TypeInfo.empty)

              reporter.info("Casting "+node.types+".asInstanceOf["+ac.tpe+"] to "+info)

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

              newNode
            }

            if (isIncompatible) {
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
                  env = BottomPTEnv
                } else {
                  // Type based equality check
                  val lhsTypes = (TypeInfo.empty /:lhsNodes) (_ union _.types)
                  val rhsTypes = (TypeInfo.empty /:rhsNodes) (_ union _.types)

                  if ((lhsTypes intersectWith rhsTypes).isEmpty) {
                    env = BottomPTEnv
                  }
                }

              case i : CFG.IfNotEqual =>
                val (_, lhsNodes) = env.getNodes(i.lhs)
                val (_, rhsNodes) = env.getNodes(i.rhs)

                // Node based inequality check
                if (lhsNodes.forall(_.isResolved) && rhsNodes.forall(_.isResolved) && lhsNodes == rhsNodes) {
                  env = BottomPTEnv
                }

              case _ =>

            }

          case bb: CFG.BasicBlock =>
            for (stmt <- bb.stmts) {
              env = apply(stmt, env, None)
            }

          case _ =>


        }

        env
      }

    }

    def preparePTCFG(fun: AbsFunction, sig: TypeSignature): FunctionCFG = {
        var cfg        = fun.cfg
        var baseEnv    = new PTEnv()

        val contSenDepth = settings.contSenDepth;

        // 1) We add 'this'/'super'
        val thisNode = LVNode(cfg.mainThisRef, sig.rec)
        baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

        for (sr <- cfg.superRefs) {
          baseEnv = baseEnv.setL(sr, Set(thisNode))
        }

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

        // 4) We add all object nodes
        for(obref <- cfg.objectRefs) {
          val n = OBNode(obref.symbol)
          baseEnv = baseEnv.addNode(n).setL(obref, Set(n))
        }

        // 5) We alter the CFG to put a bootstrapping graph step
        val bstr = cfg.newNamedVertex("bootstrap")

        for (e @ CFGEdge(_, l, v2) <- cfg.graph.outEdges(cfg.entry)) {
          cfg += CFGEdge(bstr, l, v2)
          cfg -= e
        }

        cfg += CFGEdge(cfg.entry, new CFGTrees.Effect(baseEnv, "Bootstrap of "+uniqueFunctionName(fun.symbol)) setTree fun.body, bstr)

        // 6) Finally, we map all types in the typemap
        if (!sig.tm.isEmpty) {
          cfg = new FunctionCFGInliner(Map(), sig.tm, NoUniqueID).copy(cfg)

          //dumpCFG(cfg, "prepare-last.dot")
        }


        cfg
    }

    def constructFlatCFG(fun: AbsFunction, completeCFG: FunctionCFG, effect: PTEnv): FunctionCFG = {
        var flatCFG = new FunctionCFG(fun.symbol, completeCFG.args, completeCFG.retval, true)

        flatCFG += (flatCFG.entry, new CFGTrees.Effect(effect.cleanUnreachable(completeCFG).cleanLocState(completeCFG), "Sum: "+uniqueFunctionName(fun.symbol)) setTree fun.body, flatCFG.exit)

        flatCFG
    }

    def analyzePTCFG(fun: AbsFunction, mode: AnalysisMode, sig: TypeSignature): FunctionCFG = {

      
      analysisStackSet += fun.symbol

      val cfg = getPTCFGFromFun(fun, sig)

      analysisStack     = analysisStack.push(new AnalysisContext(cfg, sig, mode))

      reporter.incIndent()

      val oldCache = preciseCallTargetsCache
      preciseCallTargetsCache = Map()

      settings.ifVerbose {
        reporter.msg("Analyzing "+fun.uniqueName+" in "+mode+" with signature "+sig+"...")
      }

      displayAnalysisContext()

      val name = uniqueFunctionName(fun.symbol)
      val dest = "last.dot"
      new CFGDotConverter(cfg, "").writeFile(dest)

      // We run a fix-point on the CFG
      val ttf = new PointToTF(fun, mode)
      val aa = new PTDataFlowAnalysis(PointToLattice, EmptyPTEnv, settings, cfg)

      ttf.analysis = aa

      try {
        aa.computeFixpoint(ttf)
      } catch {
        case aa.AINotMonotoneousException(oldEnv, newEnv, joinedEnv) =>
          joinedEnv diffWith newEnv

          sys.error("Failed to compute fixpoint due to non-monotoneous TF/Lattice!")
      }

      // Analysis CFG might have expanded
      val newCFG = aa.cfg
      val res    = aa.getResult
      val e      = res(newCFG.exit)

      //withDebugCounter { cnt =>
      //  dumpPTE(e, "effect-"+cnt+".dot");
      //}

      var reducedCFG = if (newCFG.isFlat) {
        newCFG
      } else {
        constructFlatCFG(fun, newCFG, e)
      }

      val result = if (e.isPartial) {
        assert(mode != BluntAnalysis, "Obtained non-flat PTCFG while in blunt mode")
        partialReduce(aa, fun, newCFG, res)
      } else {
        reducedCFG
      }

      settings.ifVerbose {
        reporter.msg("- Done analyzing "+fun.uniqueName)
      }

      reporter.decIndent()

      preciseCallTargetsCache = oldCache

      analysisStackSet -= fun.symbol
      analysisStack     = analysisStack.pop

      settings.ifVerbose {
        if (analysisStack.size > 0) {
          reporter.msg("... continuing analyzing "+analysisStack.top.cfg.symbol.fullName)

          withDebugCounter { cnt =>
            dumpCFG(analysisStack.top.cfg, "continue-"+cnt+".dot")
          }
        }
      }


      displayAnalysisContext()

      result
    }

    def partialReduce(aa: PTDataFlowAnalysis, fun: AbsFunction, cfg: FunctionCFG, res: Map[CFGVertex, PTEnv]): FunctionCFG = {
      // We partially reduce the result
      val unanalyzed = res(cfg.exit).danglingCalls.contains(_)

      reporter.incIndent()
      reporter.info("Reducing CFG with dangling calls: ")
      for ((aam, reason) <- res(cfg.exit).danglingCalls) {
        reporter.info("  "+aam+": "+reason)
      }
      reporter.incIndent()

      var newCFG: FunctionCFG = BasicBlocksBuilder.composeBlocks(cfg, { case e: CFG.AssignApplyMeth => unanalyzed(e) })

      val tf       = new PointToTF(fun, ReductionAnalysis)
      tf.analysis  = aa


      val cfgCopier = new CFGCopier {
        def copyStmt(stmt: CFG.Statement): CFG.Statement = stmt match {
          case aam: CFG.AssignApplyMeth if (!unanalyzed(aam)) =>
            var env = new PTEnv(isEmpty = true)

            env = tf.apply(aam, env, None)

            if (env.isBottom) {
              reporter.info("Partial reduction ended up in bottom: "+aam)
            }

            new CFG.Effect(env, "") setTree aam.getTree

          case bb: CFG.BasicBlock =>
            var env = new PTEnv(isEmpty = true)

            for (stmt <- bb.stmts) {
              env = tf.apply(stmt, env, None)
            }

            if (env.isBottom) {
              reporter.info("Partial reduction ended up in bottom: "+bb.stmts)
            }

            new CFG.Effect(env, "") setTree bb.getTree

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
            val effect = PointToLattice.join(effects: _*)

            val tree =  outs.map(_.label.tree.getOrElse(EmptyTree)).find(_ != EmptyTree).getOrElse(EmptyTree)

            newGraph += CFGEdge(v, (new CFG.Effect(effect, ""): CFG.Statement) setTree tree ,outs.head.v2)
          }

        case _ =>
          // ignore
      }

      for (e <- newGraph.E.collect{ case e @ CFGEdge(_, eff: CFG.Effect, _) if eff.env.isBottom => e }) {
        changed = true
        newGraph -= e
      }

      if (changed) {
        newCFG = newCFG.copy(graph = newGraph.immutable)
      }

      reporter.decIndent()
      reporter.info("Done.")
      reporter.decIndent()

      newCFG
    }

    def analyze(fun: AbsFunction) = {
      specializedAnalyze(fun, PreciseAnalysis, TypeSignature.fromDeclaration(fun))
    }

    def specializedAnalyze(fun: AbsFunction, mode: AnalysisMode, sig: TypeSignature) = {
      val tStart = System.currentTimeMillis

      val result = analyzePTCFG(fun, mode, sig)

      if (mode == PreciseAnalysis) {
        // We only record precise analyses here in the "official" PTCFG store
        fun.ptCFGs += sig -> (result, true)
      }

      if (result.isFlat) {
        fun.flatPTCFGs += sig -> result
        fun.flatPTCFGsTime += sig -> (fun.flatPTCFGsTime(sig)+(System.currentTimeMillis - tStart))
      }

      result
    }

    def analyzeSCC(scc: Set[Symbol]) {
      // The analysis is only run on symbols that are actually AbsFunctions, not all method symbols

      var workList = scc

      // 1) First, we remove from the worklist functions that we cannot analyze
      for (sym <- scc if !(funDecls contains sym)) {
        workList -= sym
      }

      // 2) Then, we analyze every methods until we reach a fixpoint
      while(!workList.isEmpty) {
        val sym = workList.head
        workList = workList.tail

        ptProgressBar.draw()

        if (funDecls contains sym) {
          val fun = funDecls(sym)

          val cfgBefore  = getPTCFGFromFun(fun)

          analyze(fun)

          val cfgAfter   = getPTCFGFromFun(fun)

          if (cfgBefore != cfgAfter) {
            workList ++= (simpleReverseCallGraph(sym) & scc)
          }
        }
      }
    }

    /*
    def fillDatabase() {
      if (Database.active) {
        reporter.msg("Inserting "+funDecls.size+" graph entries in the database...")

        val toInsert = for ((s, fun) <- funDecls) yield {

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

    lazy val ptProgressBar = reporter.getProgressBar(42);

    def run() {

      if (settings.onDemandMode) {
        var workList: List[AbsFunction]    = funDecls.values.filter(fun => settings.onDemandFunction(safeFullName(fun.symbol))).toList

        reporter.msg("Demand driven analysis of "+workList.size+" functions")

        if (methodProxies.size > 0) {
          workList = (methodProxies.values.toList ::: workList).distinct
          reporter.msg("Plugged "+methodProxies.size+" stubs implementations into the worklist.")

          settings.ifDebug {
            for ((s, fun) <- methodProxies) {
              reporter.msg("  "+ s.fullName +" implemented by "+fun.symbol.fullName)
            }
          }
        }


        ptProgressBar.setMax(workList.size)
        ptProgressBar.draw()

        while(!workList.isEmpty) {
          val fun = workList.head
          workList = workList.tail

          val cfgBefore  = getPTCFGFromFun(fun)
          analyze(fun)
          val cfgAfter   = getPTCFGFromFun(fun)

          if (cfgAfter != cfgBefore) {
            workList = fun :: workList
          } else {
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

      // 4) Display/dump results, if asked to
      if (!settings.dumpptgraphs.isEmpty || !settings.onDemandFunctions.isEmpty) {
        def shouldOutput(s: Symbol) = {
          settings.dumpPTGraph(safeFullName(s)) || settings.onDemandFunction(safeFullName(s))
        }

        reporter.msg(" Summary of generated effect-graphs:")

        val columns = Seq(TableColumn("Function Name", Some(40)),
                          TableColumn("Type", None),
                          TableColumn("ID", None),
                          TableColumn("DC", None),
                          TableColumn("#E", None),
                          TableColumn("ms", None),
                          TableColumn("Signature", Some(80)))

        val table = new Table(columns)

        for ((s, fun) <- funDecls.toSeq.sortBy(x => safeFullName(x._1))  if shouldOutput(s)) {
          var i = 0;
          val name = uniqueFunctionName(fun.symbol)

          val preciseCFGs = fun.ptCFGs.filter { case (_, (cfg, isAnalyzed)) => !cfg.isFlat && isAnalyzed }
          for((sig, (res, _)) <- preciseCFGs) {
            val callsRemaining = res.graph.E.filter(_.label.isInstanceOf[CFG.AssignApplyMeth]).size

            table.addRow(TableRow() | fun.symbol.fullName | "precise" | i.toString | callsRemaining.toString | "?" | "?" | sig.toString)

            val dest = safeFileName(name)+"-"+i+"-ptcfg.dot"
            new CFGDotConverter(res, "").writeFile(dest)
            i += 1
          }

          for((sig, res) <- fun.flatPTCFGs) {
            val effect = res.getFlatEffect
            val effectType = if (effect.isBottom) "bottom" else "flat"
            val nIEdges = effect.iEdges.size + effect.oEdges.size
            val time = fun.flatPTCFGsTime.getOrElse(sig, "?").toString

            table.addRow(TableRow() | fun.symbol.fullName | effectType | i.toString | "-" | nIEdges.toString | time | sig.toString)
            val dest = safeFileName(name)+"-"+i+"-ptcfg.dot"
            new PTDotConverter(effect, "").writeFile(dest)
            i += 1
          }
        }

        table.draw(s => reporter.msg(s))
      }

      //settings.drawpt match {
      //  case Some(name) =>
      //    if (Database.active) {
      //      Database.Env.lookupEnv(name).map(s => EnvUnSerializer(s).unserialize) match {
      //        case Some(e) =>
      //          val dest = name+"-pt.dot"

      //          reporter.msg("Dumping Point-To Graph to "+dest+"...")
      //          new PTDotConverter(e, "Point-to: "+name).writeFile(dest)
      //        case None =>
      //          reporter.error("Could not find "+name+" in database!")
      //      }
      //    } else {
      //      reporter.error("Could not find "+name+" in database: No database connection!")
      //    }
      //  case None =>
      //}

      if (!settings.displaypure.isEmpty) {
        reporter.title(" Purity Results:")
        for ((s, fun) <- funDecls if settings.displayPure(safeFullName(s))) {

          /*
          val (name, e, _) = getResultEnv(fun)

          val modClause = e.modifiesClause

          val modClauseString = if (modClause.isPure) {
            "@Pure"
          } else {
            def nodeToString(n: Node) = n match {
              case OBNode(s) =>
                s.name.toString
              case _ =>
                n.name
            }
            "@Modifies"+modClause.effects.map(e => nodeToString(e.root).trim+"."+e.chain.map(_.name.trim).mkString(".")).mkString("(", ", ",")")
          }

          reporter.msg(String.format("  %-40s: %s", fun.symbol.fullName, modClauseString))
          */
        }
      }
    }
  }
}

