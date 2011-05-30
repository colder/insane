package insane
package alias

import utils._
import analysis._
import CFG.ControlFlowGraph

trait PointToAnalysis extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  var predefinedPTClasses = Map[String, PTEnv]()
  var predefinedPTMethods = Map[String, PTEnv]()

  def getPTEnvFromFunSym(sym: Symbol): Option[PTEnv] = funDecls.get(sym).map(_.pointToResult)

  def getPTEnv(sym: Symbol): Option[PTEnv] = {
    getPTEnvFromFunSym(sym) orElse predefinedPTMethods.get(uniqueFunctionName(sym)) orElse predefinedPTClasses.get(uniqueClassName(sym.owner))
  }

  def getAllTargetsUsing(edges: Traversable[Edge])(from: Set[Node], via: Field): Set[Node] = {
    edges.collect{ case Edge(v1, f, v2) if (from contains v1) && (f == via) => v2 }.toSet
  }


  case class PTEnv(ptGraph: PointToGraph,
                 locState: Map[CFG.Ref, Set[Node]],
                 iEdges: Set[IEdge],
                 oEdges: Set[OEdge],
                 rNodes: Set[Node],
                 danglingCalls: Set[DCallNode],
                 isBottom: Boolean) extends dataflow.EnvAbs[PTEnv, CFG.Statement] {

    def this(isBottom: Boolean = false) = this(new PointToGraph(), Map().withDefaultValue(Set()), Set(), Set(), Set(), Set(), isBottom)

    val getAllTargets   = getAllTargetsUsing(ptGraph.E)_
    val getWriteTargets = getAllTargetsUsing(iEdges)_
    val getReadTargets  = getAllTargetsUsing(oEdges)_

    def setL(ref: CFG.Ref, nodes: Set[Node]) = {
      copy(locState = locState + (ref -> nodes), isBottom = false)
    }

    def getL(ref: CFG.Ref): Set[Node] = locState(ref)

    def removeNode(node: Node) =
      copy(ptGraph = ptGraph - node, locState = locState.map{ case (ref, nodes) => ref -> nodes.filter(_ != node)}, rNodes = rNodes - node, isBottom = false)

    def addNode(node: Node) =
      copy(ptGraph = ptGraph + node, isBottom = false)

    def addDanglingCall(dCall: DCallNode) =
      copy(danglingCalls = danglingCalls + dCall, isBottom = false)

    lazy val loadNodes: Set[LNode] = {
      ptGraph.V.collect { case l: LNode => l }
    }

    /**
     * Corresponds to:
     *   to = {..from..}.field @UniqueID
     */
    def read(from: Set[Node], field: Field, to: CFG.Ref) = {

      var res = this

      var pointResults = Set[Node]()

      for (node <- from) {
        val writeTargets = getWriteTargets(Set(node), field)

        val pointed = if (writeTargets.isEmpty) {
          getReadTargets(Set(node), field)
        } else {
          writeTargets
        }

        if (pointed.isEmpty) {
          val lNode = LNode(node, field)
          res = res.addNode(lNode).addOEdge(node, field, lNode)
          pointResults += lNode
        } else {
          pointResults ++= pointed
        }
      }

      res.setL(to, pointResults)
    }

    /**
     * Corresponds to:
     *   {..from..}.field = {..to..} @UniqueID
     */
    def write(from: Set[Node], field: Field, to: Set[Node]) = {
      assert(from.size > 0, "Writing with a empty {..from..} set!")
      assert(to.size > 0,   "Writing with a empty {..to..} set!")

      var newEnv = this

      val isStrong = from.forall(_.isSingleton) && from.size == 1

      if (isStrong) {
        // If strong update:

        // 1) We remove all previous write edges
        newEnv = newEnv.removeIEdges(from, field, getWriteTargets(from, field))

        // 2) We add back only the new write edge
        newEnv = newEnv.addIEdges(from, field, to)
      } else {
        // If weak update:

        // For each actual source node:
        for (node <- from) {
          // 1) We check for an old node reachable
          val writeTargets = getWriteTargets(Set(node), field)

          val previouslyPointed = if (writeTargets.isEmpty) {
            getReadTargets(Set(node), field)
          } else {
            writeTargets
          }

          if (previouslyPointed.isEmpty) {
            // We need to add the artificial load node, as it represents the old state
            val lNode = LNode(node, field)

            newEnv = newEnv.addNode(lNode).addOEdge(node, field, lNode).addIEdge(node, field, lNode)
          }

          // 2) We link that to node via a write edge
          newEnv.addIEdges(Set(node), field, previouslyPointed ++ to)
        }
      }

      newEnv
    }

    def addOEdge(v1: Node, field: Field, v2: Node) = addOEdges(Set(v1), field, Set(v2))

    def addOEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      var newGraph = ptGraph
      var oEdgesNew = oEdges
      for (v1 <- lv1; v2 <- lv2) {
        val e = OEdge(v1, field, v2)
        newGraph += e
        oEdgesNew += e
      }
      copy(ptGraph = newGraph, oEdges = oEdgesNew, isBottom = false)
    }

    def addIEdge(v1: Node, field: Field, v2: Node) = addIEdges(Set(v1), field, Set(v2))

    def addIEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      var newGraph = ptGraph
      var iEdgesNew = iEdges
      for (v1 <- lv1; v2 <- lv2) {
        val e = IEdge(v1, field, v2)
        newGraph += e
        iEdgesNew += e
      }
      copy(ptGraph = newGraph, iEdges = iEdgesNew, isBottom = false)
    }

    def removeIEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = iEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), iEdges = iEdges -- toRemove, isBottom = false)
    }

    def removeOEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = oEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), oEdges = oEdges -- toRemove, isBottom = false)
    }

    def setReturnNodes(ref: CFG.Ref) = {
      val nodes = getL(ref)
      copy(ptGraph = ptGraph ++ nodes, rNodes = nodes, isBottom = false)
    }

    def addGlobalNode() = {
      copy(ptGraph = ptGraph + GBNode, isBottom = false)
    }

    def duplicate = this

    def getNodes(sv: CFG.SimpleValue): Set[Node] = sv match {
      case r2: CFG.Ref => getL(r2)
      case n : CFG.Null => Set(NNode)
      case u : CFG.Unit => Set()
      case _ => Set(SNode)
    }

  }

  object BottomPTEnv extends PTEnv(true)

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    object PointToLattice extends dataflow.LatticeAbs[PTEnv, CFG.Statement] {
      val bottom = BottomPTEnv

      def join(envs: PTEnv*) = {
        /**
         * When merging environment, we need to take special care in case one
         * write edge is not present in the other envs, in that case, it
         * consists of a weak update in the resulting env.
         */

        var newIEdges = envs.flatMap(_.iEdges).toSet
        var newOEdges = envs.flatMap(_.oEdges).toSet

        // 1) We find all the pair (v1, f) that are not in every env's iEdges
        val allPairs = newIEdges.map(ed => (ed.v1, ed.label)).toSet

        val commonPairs = envs.map(_.iEdges.map(ed => (ed.v1, ed.label)).toSet).reduceRight(_ & _)

        for ((v1, field) <- allPairs -- commonPairs) {
          val lNode = LNode(v1, field)
          newIEdges += IEdge(v1, field, lNode)
          newOEdges += OEdge(v1, field, lNode)
        }

        val newGraph = (envs.map(_.ptGraph).reduceLeft(_ union _)).copy(edges = Set[Edge]() ++ newOEdges ++ newIEdges)

        new PTEnv(
          newGraph,
          envs.flatMap(_.locState.keySet).toSet.map((k: CFG.Ref) => k -> (envs.map(e => e.locState(k)).reduceRight(_ ++ _))).toMap.withDefaultValue(Set()),
          newIEdges,
          newOEdges,
          envs.flatMap(_.rNodes).toSet,
          envs.flatMap(_.danglingCalls).toSet,
          false)
      }

    }

    class PointToTF(fun: AbsFunction) extends dataflow.TransferFunctionAbs[PTEnv, CFG.Statement] {

      def apply(st: CFG.Statement, oldEnv: PTEnv): PTEnv = {
        var env = oldEnv

        def getNodesFromEnv(e: PTEnv)(sv: CFG.SimpleValue): Set[Node] = e.getNodes(sv)

        case class NodeMap(map: Map[Node, Set[Node]] = Map().withDefaultValue(Set())) extends Function1[Node, Set[Node]] {

          override def toString() = map.toString()

          def apply(n: Node): Set[Node] = map(n)

          def +(ns: (Node, Node)) = {
            copy(map = map + (ns._1 -> (map(ns._1)++Set(ns._2))))
          }

          def ++(ns: (Node, Set[Node])) = {
            copy(map = map + (ns._1 -> (map(ns._1) ++ ns._2)))
          }

          def +++(ns: Seq[(Node, Set[Node])]) = {
            copy(map = map ++ (ns.map(nn => (nn._1 -> (map(nn._1) ++ nn._2)))))
          }
        }

        def getNodes(sv: CFG.SimpleValue) = getNodesFromEnv(env)(sv)

        // Merging graphs  of callees into the caller
        def interProc(eCaller: PTEnv, target: Symbol, call: CFG.AssignApplyMeth): PTEnv = {

          def clean(env: PTEnv) = {
            env.copy(locState = Map().withDefaultValue(Set()))
          }

          def writeFixPoint(envCallee: PTEnv, envInit: PTEnv, nodeMap: NodeMap): PTEnv = {
            var env  = envInit
            var lastEnv  = env
            var i = 0

            do {
              lastEnv  = env
              i += 1
              for (IEdge(v1, field, v2) <- envCallee.iEdges) {
                env = env.write(nodeMap(v1), field, nodeMap(v2))
              }
            } while (lastEnv != env)

            env
          }

          def callerNodes(sv: CFG.SimpleValue) = getNodesFromEnv(eCaller)(sv)

          val oeCallee = getPTEnv(target)
          if (!oeCallee.isEmpty) {
            val eCallee = oeCallee.get

            val gcCallee = clean(eCallee)

            var newEnv = eCaller

            // Build map
            var nodeMap: NodeMap = NodeMap() + (GBNode -> GBNode) + (NNode -> NNode) + (SNode -> SNode)

            funDecls.get(target) match {
              case Some(fun) =>
                // Found the target function, we assign only object args to corresponding nodes
                nodeMap ++= (fun.pointToArgs(0) -> callerNodes(call.obj))  

                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex) {
                  fun.pointToArgs(i+1) match  {
                    case pNode: PNode =>
                      nodeMap ++= (pNode -> nodes)
                    case _ =>
                  }
                }

              case None =>
                // Could not find the target fun declaration, we assign args very imprecisely
                nodeMap ++= (PNode(0, AllObjects) -> callerNodes(call.obj))

                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex) {
                  nodeMap ++= (PNode(i+1, AllObjects) -> nodes)
                }
            }

            def inlineINode(iNode: INode): INode = {
              // 1) we compose a new unique id
              val callId = call.uniqueID

              val newId = iNode.pPoint match {
                case cui: CompoundUniqueID if cui.ids contains callId =>
                  cui
                case id =>
                  id add callId
              }

              // Like before, we check if the node was here
              val iNodeUnique    = INode(newId, true, iNode.types)
              val iNodeNotUnique = INode(newId, false, iNode.types)

              if ((eCaller.ptGraph.V contains iNodeUnique) || (eCaller.ptGraph.V contains iNodeNotUnique)) {
                newEnv = newEnv.removeNode(iNodeUnique).addNode(iNodeNotUnique)
                iNodeNotUnique
              } else {
                newEnv = newEnv.addNode(iNodeUnique)
                iNodeUnique
              }
            }

            // Map all inside nodes to themselves
            nodeMap +++= eCallee.ptGraph.vertices.toSeq.collect{ case n: INode => (n: Node,Set[Node](inlineINode(n))) }


            // Resolve load nodes
            def resolveLoadNode(lNode: LNode): Set[Node] = {
              val LNode(from, field) = lNode

              val fromNodes = from match {
                case l : LNode =>
                  resolveLoadNode(l)
                case _ =>
                  nodeMap(from)
              }

              var pointedResults = Set[Node]()

              for (node <- fromNodes) {
                val writeTargets = newEnv.getWriteTargets(Set(node), field)

                val pointed = if (writeTargets.isEmpty) {
                  newEnv.getReadTargets(Set(node), field)
                } else {
                  writeTargets
                }

                if (pointed.isEmpty) {
                  val lNode = LNode(node, field)
                  newEnv = newEnv.addNode(lNode).addOEdge(node, field, lNode)
                  pointedResults += lNode
                } else {
                  pointedResults ++= pointed
                }
              }

              pointedResults
            }

            for (lNode <- gcCallee.loadNodes) {
              nodeMap ++= lNode -> resolveLoadNode(lNode)
            }

            newEnv = writeFixPoint(gcCallee, newEnv, nodeMap)

            newEnv = newEnv.setL(call.r, gcCallee.rNodes flatMap nodeMap)

            newEnv
          } else {
            reporter.error("Unknown env for target "+target+" for call: "+call)
            eCaller
          }
        }

        st match {
          case av: CFG.AssignVal =>
            env = env.setL(av.r, getNodes(av.v))

          case afr: CFG.AssignFieldRead =>
            val field = SymField(afr.field)

            val fromNodes: Set[Node] = afr.obj match {
              case sr: CFG.SymRef if sr.symbol.isModule =>
                env = env.addGlobalNode
                Set(GBNode)
              case _ =>
                getNodes(afr.obj)
            }

            env = env.read(fromNodes, field, afr.r)

          case afw: CFG.AssignFieldWrite =>
            val field = SymField(afw.field)

            val fromNodes: Set[Node] = afw.obj match {
              case sr: CFG.SymRef if sr.symbol.isModule =>
                // If we do Obj.field = rhs, where Obj is a global object, rhs is escaping from the scope
                env = env.addGlobalNode
                Set(GBNode)
              case _ =>
                // Otherwise, we have obj.field = rhs
                getNodes(afw.obj)
            }

            env = env.write(fromNodes, field, getNodes(afw.rhs))

          case aam: CFG.AssignApplyMeth => // r = o.v(..args..)

            val nodes   = getNodes(aam.obj)
            val oset    = (ObjectSet.empty /: nodes) (_ ++ _.types)
            val targets = getMatchingMethods(aam.meth, oset.symbols, aam.pos)

            if (shouldInlineNow(aam.meth, oset, targets)) {
              env = PointToLattice.join(targets map (sym => interProc(env, sym, aam)) toSeq : _*)
            } else {
              val dCall = DCallNode(nodes, aam.args.map(getNodes(_)), aam.meth)
              env = env.addDanglingCall(dCall)
              env = env.setL(aam.r, Set(dCall))
            }

          case an: CFG.AssignNew => // r = new A
            val iNodeUnique    = INode(an.uniqueID, true,  ObjectSet.singleton(an.symbol))
            val iNodeNotUnique = INode(an.uniqueID, false, ObjectSet.singleton(an.symbol))

            if ((env.ptGraph.V contains iNodeUnique) || (env.ptGraph.V contains iNodeNotUnique)) {
              env = env.removeNode(iNodeUnique).addNode(iNodeNotUnique).setL(an.r, Set(iNodeNotUnique))
            } else {
              env = env.addNode(iNodeUnique).setL(an.r, Set(iNodeUnique))
            }

          case aa: CFG.AssignArray =>
            // TODO: Implement rare use-cases
            reporter.error("Ignored AssignArray:" + aa + " at "+aa.pos)

          case ac: CFG.AssignCast =>
            env = env.setL(ac.r, env.getL(ac.rhs))

          case _ =>
        }
        env
      }

    }

    def shouldInlineNow(symbol: Symbol, oset: ObjectSet, targets: Set[Symbol]) = {
      if (!oset.isExhaustive && !settings.wholeCodeAnalysis) {
        settings.ifVerbose {
          reporter.warn("Analysis of "+symbol+" delayed because of unbouded number of targets")
        }
        false
      } else if (targets.isEmpty) {
          settings.ifVerbose {
            reporter.warn("Analysis of "+symbol+" delayed because no target could be found")
          }
          false
      } else {
        val unanalyzable = targets.filter(t => getPTEnv(t).isEmpty)

        if (!unanalyzable.isEmpty) {
          settings.ifVerbose {
            reporter.warn("Analysis of "+symbol+" delayed because some targets are unanalyzable: "+unanalyzable.mkString(", "))
          }
          false
        } else {
          true
        }
      }
    }

    def analyze(fun: AbsFunction) = {
      val cfg       = fun.cfg
      var baseEnv   = new PTEnv()

      settings.ifVerbose {
        reporter.info("Analyzing "+fun.uniqueName+"...")
      }

      // 1) We add 'this' and argument nodes
      val thisNode = fun.pointToArgs(0)

      baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

      for ((a, i) <- fun.CFGArgs.zipWithIndex) {
        val pNode = fun.pointToArgs(i+1)
        baseEnv = baseEnv.addNode(pNode).setL(a, Set(pNode))
      }

      // 2) If we are in the constructor, we assign all fields defined by this class to their default value
      if (fun.symbol.name == nme.CONSTRUCTOR) {
        for (d <- fun.symbol.owner.tpe.decls if d.isValue && !d.isMethod) {
          val node = if (isGroundClass(d.tpe.typeSymbol)) { SNode } else { NNode }

          baseEnv = baseEnv.addNode(node).addIEdges(Set(thisNode), SymField(d), Set(node))
        }
      }


      // 3) We run a fix-point on the CFG
      val ttf = new PointToTF(fun)
      val aa = new dataflow.Analysis[PTEnv, CFG.Statement](PointToLattice, baseEnv, settings)

      aa.computeFixpoint(cfg, ttf)

      // 4) We retrieve the exit CFG
      val res = aa.getResult

      fun.pointToInfos  = res

      val e = res(cfg.exit).setReturnNodes(cfg.retval)

      fun.pointToResult = e

      res
    }

    def analyzeSCC(scc: Set[Symbol]) {
      // The analysis is only run on symbols that are actually AbsFunctions, not all method symbols

      var workList = scc

      // 1) First, we remove from the worklist functions that we cannot analyze
      for (sym <- scc if !(funDecls contains sym)) {
        if (getPTEnv(sym).isEmpty) {
          reporter.warn("Ignoring the analysis of unknown methods: "+sym.fullName)
        }
        workList -= sym
      }

      // 2) Then, we analyze every methods until we reach a fixpoint
      while(!workList.isEmpty) {
        val sym = workList.head
        workList = workList.tail

        if (funDecls contains sym) {
          val fun = funDecls(sym)

          val eBefore  = fun.pointToResult

          analyze(fun)

          val eAfter   = fun.pointToResult

          if (eBefore != eAfter) {
            workList ++= (simpleReverseCallGraph(sym) & scc)
          }
        }
      }
    }

    def run() {
      // 1) Fill ignore lists for pure but not analyzable classes/methods
      predefinedPTClasses += uniqueClassName(definitions.ObjectClass) -> BottomPTEnv

      // 2) Analyze each SCC in sequence, in the reverse order of their topological order
      //    We first analyze {M,..}, and then methods that calls {M,...}
      val workList = callGraphSCCs.reverse.map(scc => scc.vertices.map(v => v.symbol))
      for (scc <- workList) {
        analyzeSCC(scc)
      }

      // 3) Display/dump results, if asked to
      if (!settings.dumpptgraphs.isEmpty) {
        for ((s, fun) <- funDecls if settings.dumpPTGraph(s.fullName)) {

          val name = fun.symbol.fullName
          val cfg  = fun.cfg
          val e    = fun.pointToResult

          var newGraph = e.ptGraph

          // We complete the graph with local vars -> nodes association, for clarity
          for ((ref, nodes) <- e.locState if ref != cfg.retval; n <- nodes) {
            newGraph += VEdge(VNode(ref), n)
          }

          // We also add Dangling call information
          for (dCall <- e.danglingCalls) {
            newGraph += dCall

            for (node <- dCall.obj) {
              newGraph += DCallObjEdge(node, dCall)
            }

            for ((argNodes, i) <- dCall.args.zipWithIndex; node <- argNodes) {
              newGraph += DCallArgEdge(node, i, dCall)
            }
          }

          val dest = name+"-pt.dot"

          reporter.info("Dumping Point-To Graph to "+dest+"...")
          new PTDotConverter(newGraph, "Point-to: "+name, e.rNodes).writeFile(dest)
        }
      }
    }
  }
}

