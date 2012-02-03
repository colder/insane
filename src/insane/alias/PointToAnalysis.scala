package insane
package alias

import storage.Database

import utils._
import utils.Reporters._
import utils.Graphs.DotConverter
import CFG._

import scala.reflect.generic.Flags

trait PointToAnalysis extends PointToGraphsDefs with PointToEnvs with PointToLattices {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._


  //var predefinedPriorityEnvs = Map[Symbol, Option[PTEnv]]()

  //def getPredefPriorityEnv(sym: Symbol): Option[PTEnv] = predefinedPriorityEnvs.get(sym) match {
  //  case Some(optPTEnv) => optPTEnv
  //  case None =>
  //    if (Database.active) {
  //      val optEnv = Database.Env.lookupPriorityEnv(uniqueFunctionName(sym)).map(s => EnvUnSerializer(s).unserialize)
  //      predefinedPriorityEnvs += sym -> optEnv

  //      optEnv
  //    } else {
  //      None
  //    }
  //}

  //var predefinedEnvs = Map[Symbol, Option[PTEnv]]()

  //def getPredefEnv(sym: Symbol): Option[PTEnv] = predefinedEnvs.get(sym) match {
  //  case Some(optPTEnv) => optPTEnv
  //  case None =>
  //    if (Database.active) {
  //      val optEnv = Database.Env.lookupEnv(uniqueFunctionName(sym)).map(s => EnvUnSerializer(s).unserialize)
  //      predefinedEnvs += sym -> optEnv
  //      optEnv
  //    } else {
  //      None
  //    }
  //}

  //def getPTEnv(sym: Symbol): Option[PTEnv] = {
  //  getPredefPriorityEnv(sym) orElse getPTEnvFromFunSym(sym) orElse getPredefEnv(sym)
  //}

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    var cnt = 0

    var indent = 0

    def incIndent() {
      indent += 2
    }

    def decIndent() {
      indent -= 2
    }

    def curIndent = {
      " "*indent
    }

    var predefinedStaticCFGs = Map[Symbol, Option[FunctionCFG]]()
    def getPredefStaticCFG(sym: Symbol) = predefinedStaticCFGs.get(sym) match {
      case Some(optcfg) =>
        optcfg

      case None =>
        val optcfg = uniqueFunctionName(sym) match {
          case "java.lang.Object.<init>(()java.lang.Object)" |
               "java.lang.Object.$eq$eq((x$1: java.lang.Object)Boolean)" |
               "scala.Int.$eq$eq((x: Int)Boolean)" |
               "scala.Int.$minus((x: Int)Int)" |
               "java.lang.Object.$bang$eq((x$1: java.lang.Object)Boolean)" =>

            val (args, argsTypes, retval) = sym.tpe match {
              case MethodType(argssym, tpe) =>
                (argssym.map(s => new CFGTrees.SymRef(s, NoUniqueID)), argssym.map(s => ObjectSet.subtypesOf(s.tpe)), new CFGTrees.TempRef("retval", NoUniqueID, tpe))

              case tpe =>
                (Seq(), Seq(), new CFGTrees.TempRef("retval", NoUniqueID, tpe))
            }

            var cfg = new FunctionCFG(sym, args, retval, true)

            var baseEnv    = new PTEnv()

            // 1) We add 'this'/'super'
            val thisNode = LVNode(cfg.mainThisRef, ObjectSet.subtypesOf(cfg.mainThisRef.tpe))
            baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

            // 2) We add arguments
            for ((a, oset) <- cfg.args zip argsTypes) {
              val aNode = if (isGroundOSET(oset)) {
                  typeToLitNode(oset.exactTypes.head)
                } else {
                  LVNode(a, oset)
                }
              baseEnv = baseEnv.addNode(aNode).setL(a, Set(aNode))
            }

            val retNode = typeToLitNode(retval.tpe)
            baseEnv = baseEnv.addNode(retNode).setL(retval, Set(retNode))

            cfg += (cfg.entry, new CFGTrees.Effect(baseEnv, "Bootstrap of "+uniqueFunctionName(sym)), cfg.exit)
            Some(cfg)

          case _ =>
            None
        }

        predefinedStaticCFGs += sym -> optcfg

        optcfg
    }

    object PTAnalysisModes extends Enumeration {
      val PreciseAnalysis = Value("PreciseAnalysis")
      val BluntAnalysis   = Value("BluntAnalysis")
    }

    type AnalysisMode = PTAnalysisModes.Value
    import PTAnalysisModes._


    def getPTCFGFromFun(fun: AbsFunction): FunctionCFG = {
      getPTCFGFromFun(fun, declaredArgsTypes(fun))
    }

    def getPTCFGFromFun(fun: AbsFunction, argsTypes: Seq[ObjectSet]): FunctionCFG = {
      getPTCFGResultFromFun(fun, argsTypes)._1
    }


    def getPTCFGResultFromFun(fun: AbsFunction, argsTypes: Seq[ObjectSet]): (FunctionCFG, Boolean) = {
      val actualArgsTypes = clampReceiverType(fun.symbol, argsTypes)

      // Is the PTCFG for this signature already ready?
      fun.ptCFGs.get(actualArgsTypes) match {
        case Some(result) =>
          // Yes.
          result
        case None =>
          // No, we prepare a fresh one given the types, and store it.
          val result = (preparePTCFG(fun, actualArgsTypes), false)
          fun.ptCFGs += actualArgsTypes -> result
          result
      }
    }

    def getPTCFG(sym: Symbol, argsTypes: Seq[ObjectSet]): Option[FunctionCFG] = {
      funDecls.get(sym) match {
        case Some(fun) =>
          Some(getPTCFGFromFun(fun, argsTypes))
        case None =>
          getPredefStaticCFG(sym)
      }
    }

    def getPTCFGAnalyzed(sym: Symbol, callgraphSCC: Set[Symbol], argsTypes: Seq[ObjectSet]): Option[FunctionCFG] = {
      funDecls.get(sym) match {
        case Some(fun) =>
          Some(getPTCFGAnalyzedFromFun(fun, callgraphSCC, argsTypes))
        case None =>
          getPredefStaticCFG(sym)
      }
    }

    def getPTCFGAnalyzedFromFun(fun: AbsFunction, callGraphSCC: Set[Symbol], argsTypes: Seq[ObjectSet]): FunctionCFG = {
      val actualArgsTypes = clampReceiverType(fun.symbol, argsTypes)

      getPTCFGResultFromFun(fun, actualArgsTypes) match {
        case (cfg, true) =>
          cfg
        case (cfg, false) =>
          specializedAnalyze(fun, callGraphSCC, PreciseAnalysis, actualArgsTypes)
      }
    }

    def clampReceiverType(sym: Symbol, argsTypes: Seq[ObjectSet]): Seq[ObjectSet] = {
      //TODO: use intersect here?
      if (argsTypes.head.isSubTypeOf(sym.owner.tpe)) {
        // It's precise enough
        argsTypes
      } else {
        Seq(ObjectSet.subtypesOf(sym.owner.tpe)) ++ argsTypes.tail
      }
    }

    def getFlatPTCFG(sym: Symbol, argsTypes: Seq[ObjectSet]): Option[FunctionCFG] = {
      val actualArgsTypes = clampReceiverType(sym, argsTypes)

      val res = funDecls.get(sym) match {
        case Some(fun) =>
          fun.flatPTCFGs.get(actualArgsTypes) match {
            case Some(flatPTCFG) =>
              // Already here? Nice.
              Some(flatPTCFG)
            case _ =>
              // We need to re-analyze in blunt-mode
              var changed = false;

              settings.ifDebug {
                reporter.info(curIndent+"Performing blunt fixpoint on "+sym.fullName+" with types: "+actualArgsTypes.mkString(", "))
              }

              // We prevent infinite recursion by assigning a bottom effect by default.
              fun.flatPTCFGs += actualArgsTypes -> constructFlatCFG(fun, getPTCFGFromFun(fun, argsTypes), EmptyPTEnv)

              def computeFlatEffect() = {
                // Here the receiver might be a supertype of the method owner, we restrict in this case:

                val cfg = specializedAnalyze(fun, Set(), BluntAnalysis, actualArgsTypes)

                assert(cfg.isFlat, "CFG Returned is not Flat!")

                val effect = cfg.graph.E.head.label match {
                  case e: CFGTrees.Effect =>
                    e.env
                  case _ =>
                    sys.error("Flat CFG does not contain edge with Effects!")
                }

                (cfg, effect)
              }

              var (oldCFG, oldEffect) = computeFlatEffect()

              do {
                var (newCFG, newEffect) = computeFlatEffect()
                val joinEffect = PointToLattice.join(oldEffect, newEffect)

                changed = (newEffect != oldEffect)
                if (false && changed) {
                  println(" Before : =================================")
                  println(oldEffect)
                  println(" After  : =================================")
                  println(newEffect)
                  println(" Diff   : =================================")
                  oldEffect diffWith newEffect
                }
                oldCFG    = newCFG;
                oldEffect = newEffect;
              } while(changed);

              fun.flatPTCFGs += actualArgsTypes -> oldCFG

              Some(oldCFG)
          }
        case None =>
          getPredefStaticCFG(sym)
      }

      res match {
        case Some(ptCFG) =>
          assert(ptCFG.isFlat, "Reduced CFG obtained is not actually flat")
          Some(ptCFG)
        case None =>
          None
      }
    }



    class PointToTF(fun: AbsFunction, callGraphSCC: Set[Symbol], analysisMode: AnalysisMode) extends dataflow.TransferFunctionAbs[PTEnv, CFG.Statement] {

      var analysis: dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG] = null

      /*
       * Heuristic to decide to use flat effects to inline symbol.
       * The return value is used as follows:
       *  - true:  we obtain the definite effect from that call by
       *           possibly reanalyzing it and inline this flat effect.
       *  - false: we inline by CFG
       */
      def shouldUseFlatInlining(symbol: Symbol, callArgs: Seq[ObjectSet], targets: Set[Symbol]): Boolean = {
        callArgs.forall(_.resolveTypes.size == 1)
      }


      def isRecursive(symbol: Symbol) = {
        (simpleCallGraph(symbol) contains symbol) || ((methCallSCC contains symbol) && methCallSCC(symbol).size > 1)
      }

      /*
       * Heuristic to decide how and when to inline
       */
      def shouldWeInlineThis(symbol: Symbol, callArgs: Seq[ObjectSet], targets: Set[Symbol], excludedTargets: Set[Symbol]): Either[(Set[FunctionCFG], AnalysisMode), (String, Boolean)] = {

        val targetsToConsider = targets -- excludedTargets

        analysisMode match {
          case PreciseAnalysis =>
            if (targets.isEmpty) {
              Right("no target could be found", true)
            } else {
              val receiverTypes = callArgs.head

              if (!receiverTypes.isExhaustive && !settings.assumeClosedWorld) {
                Right("unbouded number of targets", true)
              } else {
                if (targetsToConsider.size > 3) {
                  Right("too many targets ("+targetsToConsider.size+")", false)
                } else {
                  val unanalyzable = targetsToConsider.filter(t => getPTCFG(t, callArgs).isEmpty)

                  if (!unanalyzable.isEmpty) {
                    Right("some targets are unanalyzable: "+unanalyzable.map(uniqueFunctionName(_)).mkString(", "), true)
                } else if (targetsToConsider.exists( t => isRecursive(t) && !shouldUseFlatInlining(t, callArgs, targets))) {
                    Right("Recursive calls should stay as-is in precise mode", false)
                  } else {
                    val availableTargets = targetsToConsider flatMap { sym =>
                      val ptCFG = if (shouldUseFlatInlining(sym, callArgs, targets)) {
                        getFlatPTCFG(sym, callArgs)
                      } else {
                        getPTCFGAnalyzed(sym, callGraphSCC, callArgs)
                      }

                      ptCFG match {
                        case None =>
                          reporter.error(curIndent+"Could not gather pt-CFG of "+sym.name+" ("+uniqueFunctionName(sym)+"), ignoring.")
                          None
                        case cfg =>
                          cfg
                      }
                    }

                    Left((availableTargets, if (availableTargets forall (_.isFlat)) BluntAnalysis else PreciseAnalysis))
                  }
                }
              }
            }
          case BluntAnalysis =>
            // We have to analyze this, and thus inline, no choice
            // here, we require that the result is a flat effect
            Left((targetsToConsider flatMap { getFlatPTCFG(_, callArgs) }, BluntAnalysis))
        }
      }
      def apply(edge: CFGEdge[CFG.Statement], oldEnv: PTEnv, scc: SCC[CFGVertex]): PTEnv = {
        if (oldEnv.isBottom) {
          return oldEnv
        }

        val st  = edge.label

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
            copy(map = map + (ns._1 -> (map(ns._1) ++ ns._2)))
          }

          def +++(ns: Traversable[(Node, Set[Node])]) = {
            copy(map = map ++ (ns.map(nn => (nn._1 -> (map(nn._1) ++ nn._2)))))
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

              // check nodes mapping compatibility
              val newOuterNodes = outerNodes.flatMap(outerNode => {
                // Todo: Improve compatibility check
                val refinedOset = if (innerNode.types != outerNode.types) {
                  ObjectSet(innerNode.types intersectWith outerNode.types, outerNode.types.isExhaustive)
                } else {
                  outerNode.types
                }

                val isRefined = refinedOset != outerNode.types

                outerNode match {
                  case _ if refinedOset.isEmpty =>
                    None

                  case n @ LNode(from, field, pPoint, types) if isRefined =>
                    val node = LNode(from, field, pPoint, refinedOset)
                    newOuterG = newOuterG.splitNode(n, node)
                    Some(node)

                  case n @ LVNode(ref, types) if isRefined =>
                    val node = LVNode(ref, refinedOset)
                    newOuterG = newOuterG.splitNode(n, node)
                    Some(node)

                  case n =>
                    Some(n)
                }
              })

              nodeMap ++= (innerNode -> newOuterNodes)
            }

            var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, uniqueID, pos, allowStrongUpdates)

            for ((r, nodes) <- innerG.locState) {
              newOuterG2 = newOuterG2.setL(r, nodes flatMap newNodeMap)
            }

            newOuterG2
          }
        }

        def mergeGraphsWithMap(outerG: PTEnv, innerG: PTEnv, nodeMapInit: NodeMap, uniqueID: UniqueID, pos: Position, allowStrongUpdates: Boolean): (PTEnv, NodeMap) = {
          cnt += 1

          // Build map
          var newOuterG = outerG;
          var nodeMap   = nodeMapInit;

          // 1) We build basic nodemap
          for (n <- GBNode :: NNode :: NNode :: TrueLitNode :: FalseLitNode :: BooleanLitNode :: LongLitNode :: DoubleLitNode :: StringLitNode :: IntLitNode :: ByteLitNode :: CharLitNode :: FloatLitNode :: ShortLitNode :: Nil if innerG.ptGraph.V.contains(n)) {
            nodeMap += n -> n
          }

          // 2) We add all singleton object nodes to themselves
          for (n <- innerG.ptGraph.V.filter(_.isInstanceOf[OBNode])) {
            nodeMap += n -> n
          }

          // 4) Inline Inside nodes with refinement of the allocation site
          def inlineINode(iNode: INode): INode = {
            // 1) we compose a new unique id
            val callId = uniqueID

            val newId = iNode.pPoint safeAdd callId

            // Like before, we check if the node was here
            val iNodeUnique    = INode(newId, true, iNode.types)
            val iNodeNotUnique = INode(newId, false, iNode.types)

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
            val LNode(_, field, pPoint, types) = lNode

            val innerFromNodes = innerG.ptGraph.ins(lNode).collect{ case OEdge(f, _, _) => f }

            val fromNodes = innerFromNodes map ( n => (n, n match {
              case from : LNode if !stack(from) =>
                resolveLoadNode(from, stack + lNode) ++ nodeMap(from)
              case from =>
                nodeMap(from)
            }))

            var pointedResults = Set[Node]()

            for ((innerFromNode, tmpNodes) <- fromNodes; tmpNode <- tmpNodes) {

              // Todo: Improve compatibility check
              val refinedOset = if (tmpNode.types != innerFromNode.types) {
                ObjectSet(tmpNode.types intersectWith innerFromNode.types, tmpNode.types.isExhaustive)
              } else{
                tmpNode.types
              }

              val isRefined = refinedOset != tmpNode.types

              val optnode = tmpNode match {
                case _ if refinedOset.isEmpty =>
                  None

                case n @ LNode(from, field, pPoint, types) if isRefined =>
                  val node = LNode(from, field, pPoint, refinedOset)
                  newOuterG = newOuterG.splitNode(n, node)
                  Some(node)
                case n @ LVNode(ref, types) if isRefined =>
                  val node = LVNode(ref, refinedOset)
                  newOuterG = newOuterG.splitNode(n, node)
                  Some(node)

                case n =>
                  Some(n)
              }

              optnode match {
                case Some(node) =>
                  val writeTargets = newOuterG.getWriteTargets(Set(node), field)

                  val pointed = if (writeTargets.isEmpty) {
                    newOuterG.getReadTargets(Set(node), field)
                  } else {
                    writeTargets
                  }

                  if (pointed.isEmpty) {
                    node match {
                      case i: INode =>
                        /**
                         * this loadNode is mapped to an insideNode, innerG must
                         * have a write target for it, it will be brough to
                         * newOuterG later
                         */
                      case _ =>
                        val newId = pPoint safeAdd uniqueID

                        safeLNode(node, field, newId) match {
                          case Some(lNode) =>
                            val nodesToAdd = Set(lNode) ++ newOuterG.ptGraph.V.collect{ case l: LNode if (l.fromNode, l.via, l.pPoint) == (lNode.fromNode, lNode.via, lNode.pPoint) => l }

                            for (nodeToAdd <- nodesToAdd) {
                              newOuterG = newOuterG.addNode(nodeToAdd).addOEdge(node, field, nodeToAdd)
                              pointedResults += nodeToAdd
                            }
                          case None =>
                            reporter.error("This shouldn't occur anymore!")
                        }
                    }
                  } else {
                    pointedResults ++= pointed
                  }
                case None =>
                  // Ignore incompatibility
              }
            }

            pointedResults
          }

          // Filter node mappings with incoherent types
          def removeInconsistencies() {
            for ((innerNode, outerNodes) <- nodeMap.map) {

              val innerTypes = innerNode.types

              var toRemove = Set[Node]()
              for (n <- outerNodes) {
                if (n.types != innerTypes && n.types.intersectWith(innerTypes).isEmpty) {
                  toRemove += n
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

            removeInconsistencies()

            for (lNode <- innerG.loadNodes) {
              nodeMap ++= lNode -> resolveLoadNode(lNode)
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
            assert(!nodes.isEmpty, "IMPOSSIBRU! Could not find any node for the receiver: "+aam)

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

            val oset = aam.obj match {
              case CFG.SuperRef(sym, _) =>
                ObjectSet.singleton(sym.superClass.tpe)
              case _ =>
                (ObjectSet.empty /: nodes) (_ ++ _.types)
            }

            val callArgsTypes = Seq(oset) ++ (for (a <- aam.args) yield {
              val (tmp, nodes) = newEnv.getNodes(a)
              newEnv = tmp
              (ObjectSet.empty /: nodes) (_ ++ _.types)
            })

            val targets = getMatchingMethods(aam.meth, oset.resolveTypes, aam.pos, aam.isDynamic)

            shouldWeInlineThis(aam.meth, callArgsTypes, targets, aam.excludedSymbols) match {
              case Left((targetCFGs, PreciseAnalysis)) => // We should inline this precisely
                var cfg = analysis.cfg

                // 1) Remove current edge

                settings.ifDebug {
                  reporter.debug(curIndent+"Ready to precise-inline for : "+aam+", "+targetCFGs.size+" targets available: "+targetCFGs.map(_.symbol.fullName)+" for receiver "+nodes)
                }

                val nodeA = edge.v1
                val nodeB = edge.v2


                if (targetCFGs.size == 0) {
                  // We could not inline anything, continue as if the call did nothing
                  // No restart
                } else {
                  // We replace the call node with a call node tracking the inlined targets
                  cfg -= edge
                  cfg += new CFGEdge(nodeA, aam.excludeSymbols(targetCFGs.map(_.symbol)), nodeB)

                  for (targetCFG <- targetCFGs) {

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
                          reporter.error(curIndent+"  Unnexpected non-ref for the receiver!", aam.pos)
                    }

                    // c) mapping retval
                    map += targetCFG.retval -> aam.r

                    // 3) Rename targetCFG
                    val renamedCFG = new FunctionCFGRefRenamer(map, aam.uniqueID).copy(targetCFG)

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
                    reporter.debug(curIndent+"  Restarting analysis of "+fun.symbol.fullName+"...")
                  }

                  cnt += 1

                  cfg = cfg.removeSkips.removeIsolatedVertices

                  analysis.restartWithCFG(cfg)
                }

              case Left((targetCFGs, BluntAnalysis)) => // We should inline this in a blunt fashion

                settings.ifDebug {
                  reporter.debug(curIndent+"Ready to blunt-inline for : "+aam+", "+targetCFGs.size+" targets available: "+targetCFGs.map(_.symbol.fullName)+" for receiver "+nodes)
                }

                val envs = targetCFGs.map { targetCFG =>
                  val innerG    = targetCFG.getFlatEffect;

                  if (innerG.isEmpty) {
                    env
                  } else {
                    /**
                     * In an inlining merge graph, we map local variables for
                     * args and return values, once those are mapped correctly,
                     * we proceed with mergeGraph as usual.
                     */
                    var refMap    = Map[CFG.Ref, CFG.SimpleValue]();

                    // 1) Mapping refs:
                    //   a) mapping args
                    refMap ++=  targetCFG.args zip aam.args

                    //   b) mapping receiver
                    aam.obj match {
                        case r: CFGTrees.Ref =>
                          refMap += targetCFG.mainThisRef -> r

                        case _ =>
                          reporter.error(curIndent+"  Unnexpected non-ref for the receiver!", aam.pos)
                    }

                    //   c) mapping retval
//                    refMap += targetCFG.retval -> aam.r

                    // 2) Build an index of the inner LV nodes
                    def findInnerNodes(r: CFG.Ref) = innerG.locState(r) match {
                      case ns if ns.isEmpty =>
                        reporter.error(curIndent+"  Unable to find inner nodes corresponding to "+r+" while inlining "+aam+" with target: "+targetCFG.symbol.fullName, aam.pos)

                        println("Target retval:   "+targetCFG.retval)
                        println("Target graph:    "+targetCFG.graph)
                        println("Target locState: "+innerG.locState)
                        println("Target effect:   "+innerG)

                        sys.exit(1);

                      case nodes =>
                        nodes
                    }

                    // 3) We apply the same mapping but on corresponding nodes:
                    var newOuterG = env;
                    var nodeMap   = NodeMap();

                    for ((iRef, oRef) <- refMap) {
                      val (newOG, outerNodes) = newOuterG.getNodes(oRef)

                      val innerNodes = findInnerNodes(iRef)

                      newOuterG = newOG

                      nodeMap +++= innerNodes.map(_ -> outerNodes)
                    }

                    var (newOuterG2, newNodeMap) = mergeGraphsWithMap(newOuterG, innerG, nodeMap, aam.uniqueID, aam.pos, true)

                    val mappedRet = innerG.locState(targetCFG.retval) flatMap newNodeMap
                    // We still need to modify the locstate for the return value
                    newOuterG2 = newOuterG2.setL(aam.r, mappedRet)

                    if (mappedRet.isEmpty) {
                      val dest = "err.dot"
                      new PTDotConverter(innerG, "Flat Effect").writeFile(dest)
                      reporter.error("Woops!?! retvals are empty: "+ targetCFG.retval+" innerlocstate: "+innerG.locState(targetCFG.retval))
                      sys.error("Bleh");
                    }


                    newOuterG2
                  }
                }


                env = PointToLattice.join(envs.toSeq : _*)

              case Right((reason, isError)) =>
                aam.obj match {
                  case CFG.SuperRef(sym, _) =>
                    reporter.error(List(
                      curIndent+"Cannot inline/delay call to super."+sym.name+" ("+uniqueFunctionName(sym)+"), ignoring call.",
                      curIndent+"Reason: "+reason), aam.pos)

                    // From there on, the effects are partial graphs
                    env = new PTEnv(true, false)
                  case _ =>
                    if (isError) {
                      reporter.error(List(
                        curIndent+"Cannot inline/delay call "+aam+", ignoring call.",
                        curIndent+"Reason: "+reason), aam.pos)
                    } else {
                      settings.ifDebug {
                        reporter.debug(List(
                          curIndent+"Delaying call to "+aam+"",
                          curIndent+"Reason: "+reason), aam.pos)
                      }
                    }

                    // From there on, the effects are partial graphs
                    env = new PTEnv(true, false)
                }
          }
          case an: CFG.AssignNew => // r = new A
            val iNodeUnique    = INode(an.uniqueID, true,  ObjectSet.singleton(an.tpe))
            val iNodeNotUnique = INode(an.uniqueID, false, ObjectSet.singleton(an.tpe))

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

            val newNodes = for (node <- nodes) yield {
              val types = ac.tpe match {
                case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
                  Set(ac.tpe)

                case tpe =>
                  node.types.intersectWith(tpe)
              }

              if (types.isEmpty) {
                isIncompatible = true
              }


              val oset = ObjectSet(types, node.types.isExhaustive)

              val newNode = node match {
                case LVNode(ref, _) =>
                  LVNode(ref, oset)
                case LNode(fromNode, via, pPoint, _) =>
                  LNode(fromNode, via, pPoint, oset)
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

                val newNodes: Set[Node] = if (nodes  == Set(FalseLitNode)) {
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

                val newNodes: Set[Node] = if (nodes  == Set(TrueLitNode)) {
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
                  val lhsTypes = (ObjectSet.empty /:lhsNodes) (_ ++ _.types)
                  val rhsTypes = (ObjectSet.empty /:rhsNodes) (_ ++ _.types)

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
          case _ =>


        }

        env
      }

    }

    def preparePTCFG(fun: AbsFunction, argsTypes: Seq[ObjectSet]): FunctionCFG = {
        var cfg        = fun.cfg
        var baseEnv    = new PTEnv()

        // 1) We add 'this'/'super'
        val thisNode = LVNode(cfg.mainThisRef, argsTypes.head)
        baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

        for (sr <- cfg.superRefs) {
          baseEnv = baseEnv.setL(sr, Set(thisNode))
        }

        // 2) We add arguments
        for ((a, oset) <- cfg.args zip argsTypes.tail) {
          val aNode = if (isGroundOSET(oset)) {
              typeToLitNode(oset.exactTypes.head)
            } else {
              LVNode(a, oset)
            }
          baseEnv = baseEnv.addNode(aNode).setL(a, Set(aNode))
        }

        // 3) We add retval
        val retType = ObjectSet.subtypesOf(cfg.retval.tpe)

        val retNode = if (isGroundOSET(retType)) {
          typeToLitNode(cfg.retval.tpe)
        } else {
          LVNode(cfg.retval, retType)
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

        cfg
    }

    def constructFlatCFG(fun: AbsFunction, completeCFG: FunctionCFG, effect: PTEnv): FunctionCFG = {
        var flatCFG = new FunctionCFG(fun.symbol, completeCFG.args, completeCFG.retval, true)

        flatCFG += (flatCFG.entry, new CFGTrees.Effect(effect.cleanUnreachable(flatCFG).cleanLocState(flatCFG), "Sum: "+uniqueFunctionName(fun.symbol)) setTree fun.body, flatCFG.exit)

        flatCFG
    }

    def analyzePTCFG(fun: AbsFunction, callGraphSCC: Set[Symbol], mode: AnalysisMode, argsTypes: Seq[ObjectSet]): FunctionCFG = {

      val cfg = getPTCFGFromFun(fun, argsTypes)

      settings.ifVerbose {
        reporter.msg(curIndent+"Analyzing "+fun.uniqueName+" in "+mode+" with types "+argsTypes.mkString(", ")+"...")
      }
      incIndent()

      // We run a fix-point on the CFG
      val ttf = new PointToTF(fun, callGraphSCC, mode)
      val aa = new dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG](PointToLattice, EmptyPTEnv, settings, cfg)

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

      var reducedCFG = if (newCFG.isFlat) {
        newCFG
      } else {
        constructFlatCFG(fun, newCFG, e)
      }

      val result = if (e.isPartial) {
        assert(mode != BluntAnalysis, "Obtained non-flat PTCFG while in blunt mode")
        partialReduce(newCFG)
      } else {
        reducedCFG
      }

      decIndent()
      settings.ifVerbose {
        reporter.msg(curIndent+"Done analyzing "+fun.uniqueName+".")
      }
      result
    }

    def partialReduce(cfg: FunctionCFG): FunctionCFG = {
      // We partially reduce the result
      cfg
    }

    def declaredArgsTypes(fun: AbsFunction): Seq[ObjectSet] = {
     ObjectSet.subtypesOf(fun.symbol.owner.tpe) +: fun.args.map(a => ObjectSet.subtypesOf(a.tpt.tpe));
    }

    def analyze(fun: AbsFunction, callGraphSCC: Set[Symbol]) = {
      specializedAnalyze(fun, callGraphSCC, PreciseAnalysis, declaredArgsTypes(fun))
    }

    def specializedAnalyze(fun: AbsFunction, callGraphSCC: Set[Symbol], mode: AnalysisMode, argsTypes: Seq[ObjectSet]) = {
      val result = analyzePTCFG(fun, callGraphSCC, mode, argsTypes)



      if (mode == PreciseAnalysis) {
        // We only record precise analyses here in the "official" PTCFG store
        fun.ptCFGs += argsTypes -> (result, true)
      }

      if (result.isFlat) {
        fun.flatPTCFGs += argsTypes -> result
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

          analyze(fun, scc)

          val cfgAfter   = getPTCFGFromFun(fun)

          if (cfgBefore != cfgAfter) {
            workList ++= (simpleReverseCallGraph(sym) & scc)
          }
        }
      }
    }

    def abstractsClassAnnotation(symbol: Symbol): Option[Symbol] = {
      symbol.annotations.find(_.atp.safeToString startsWith "insane.annotations.Abstracts") match {
          case Some(annot) =>

            annot.args match {
              case List(l: Literal) =>
                val name = l.value.stringValue

                try {
                  annot.atp.safeToString match {
                    case "insane.annotations.AbstractsClass" =>
                      Some(definitions.getClass(name))
                    case "insane.annotations.AbstractsModuleClass" =>
                      Some(definitions.getModule(name).moduleClass)
                    case _ =>
                      reporter.error(curIndent+"Could not understand annotation: "+annot, symbol.pos)
                      None
                  }
                } catch {
                  case e =>
                    reporter.error(curIndent+"Unable to find class symbol from name "+name+": "+e.getMessage)
                    None
                }
              case _ =>
                reporter.error(curIndent+"Could not understand annotation: "+annot, symbol.pos)
                None
            }
          case None =>
            None
        }
    }

    def abstractsMethodAnnotation(symbol: Symbol): Option[String] = {
      symbol.annotations.find(_.atp.safeToString == "insane.annotations.AbstractsMethod") match {
          case Some(annot) =>

            annot.args match {
              case List(l: Literal) => Some(l.value.stringValue)
              case _ =>
                reporter.error(curIndent+"Could not understand annotation: "+annot, symbol.pos)
                None
            }
          case None =>
            None
        }
    }

    /*
    def getResultEnv(fun: AbsFunction): (String, PTEnv, Boolean) = {
      // We get the name of the method in the annotation, if any
      var isSynth = false

      var env = fun.pointToResult

      var name = abstractsMethodAnnotation(fun.symbol) match {
        case Some(n) =>
          isSynth = true

          if (fun.body == EmptyTree) {
            // In case the function was abstract with a method annotation, we
            // generate its return value node

            //val iNode = INode(new UniqueID(0), true, methodReturnType(fun.symbol))
            //env = env.addNode(iNode).copy(rNodes = Set(iNode))
            //fun.pointToResult = env
            sys.error("TODO")
          }
          n
        case None =>
          uniqueFunctionName(fun.symbol)
      }

      // We check if the class actually contains the Abstract annotation, in
      // which case we need to fix types of nodes.
      abstractsClassAnnotation(fun.symbol.owner) match {
        case Some(newClass) =>
          val oldClass = fun.symbol.owner
          // We need to replace references to fun.symbol.owner into symbol
          env = new PTEnvReplacer(Map(oldClass.tpe -> newClass.tpe), Map(oldClass -> newClass)).copy(env)
          isSynth = true

        case None =>
      }

      (name, env, isSynth)
    }
    */

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
      // 1) Analyze each SCC in sequence, in the reverse order of their topological order
      //    We first analyze {M,..}, and then methods that calls {M,...}
      var workList = callGraphSCCs.reverse.map(scc => scc.vertices.map(v => v.symbol))

      if (settings.onDemandMode) {
        workList  = workList.filter(scc => scc.exists(s => settings.onDemandFunction(safeFullName(s))))
        reporter.msg("The following functions will be analyzed: "+workList.map(_.map(_.fullName).mkString(", ")).mkString(", "))
      }

      val totJob   = workList.map(_.size).sum

      ptProgressBar.setMax(totJob)
      ptProgressBar.draw()

      for (scc <- workList) {
        analyzeSCC(scc)
        ptProgressBar ticks scc.size
        ptProgressBar.draw()
      }

      ptProgressBar.end();

      // 2) Fill graphs in the DB, if asked to
      //if (settings.fillGraphs && !settings.fillGraphsIteratively) {
      //  fillDatabase()
      //}

      // 4) Display/dump results, if asked to
      if (!settings.dumpptgraphs.isEmpty) {
        reporter.msg(" Summary of generated effect-graphs:")

        val columns = Seq(TableColumn("Function Name", Some(40)),
                          TableColumn("Type", None),
                          TableColumn("ID", None),
                          TableColumn("Signature", Some(80)))

        val table = new Table(columns)
          for ((s, fun) <- funDecls.toSeq.sortBy(x => safeFullName(x._1))  if settings.dumpPTGraph(safeFullName(s))) {
          var i = 0;
          val name = uniqueFunctionName(fun.symbol)

          val ptCFG = getPTCFGFromFun(fun)
          val dest = name+"-ptcfg.dot"
          new CFGDotConverter(ptCFG, "Point-to-CFG: "+name).writeFile(dest)

          val preciseCFGs = fun.ptCFGs.filter { case (_, (cfg, isAnalyzed)) => !cfg.isFlat && isAnalyzed }
          for((args, (res, _)) <- preciseCFGs) {

            table.addRow(TableRow() | fun.symbol.fullName | "precise" | i.toString | args.mkString(", "))

            val dest = name+"-"+i+"-ptcfg.dot"
            new CFGDotConverter(res, "Point-to-CFG: "+name).writeFile(dest)
            i += 1
          }

          for((args, res) <- fun.flatPTCFGs) {
            table.addRow(TableRow() | fun.symbol.fullName | "flat" | i.toString | args.mkString(", "))
            val dest = name+"-"+i+"-ptcfg.dot"
            new PTDotConverter(res.getFlatEffect, "Flat Effect: "+name).writeFile(dest)
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

