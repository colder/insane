package insane
package alias

import utils._
import analysis._
import CFG.ControlFlowGraph

trait PointToAnalysis extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  var predefinedPTClasses = Map[Symbol, PTEnv]()
  var predefinedPTMethods = Map[Symbol, PTEnv]()

  def getPTEnvFromFunSym(sym: Symbol): Option[PTEnv] = funDecls.get(sym).map(_.pointToResult)

  def getPTEnv(sym: Symbol): Option[PTEnv] = {
    getPTEnvFromFunSym(sym) orElse predefinedPTMethods.get(sym) orElse predefinedPTClasses.get(sym.owner)
  }

  case class PTEnv(ptGraph: PointToGraph,
                 locState: Map[CFG.Ref, Set[Node]],
                 iEdges: Set[IEdge],
                 oEdges: Set[OEdge],
                 eNodes: Set[Node],
                 rNodes: Set[Node],
                 isBottom: Boolean) extends DataFlowEnvAbs[PTEnv, CFG.Statement] {

    def this(isBottom: Boolean = false) = this(new PointToGraph(), Map().withDefaultValue(Set()), Set(), Set(), Set(), Set(), isBottom)

    def union(that: PTEnv) = {
      copy(ptGraph union that.ptGraph,
        (locState.keySet ++ that.locState.keySet).map(k => k -> (locState(k)++that.locState(k))).toMap.withDefaultValue(Set()),
        iEdges union that.iEdges,
        oEdges union that.oEdges,
        eNodes union that.eNodes,
        rNodes union that.rNodes,
        false)
    }

    def reachableNodes(from: Set[Node], via: Set[Edge]): Set[Node] = {
      var res = from
      var queue = from

      while(!queue.isEmpty) {
        val n = queue.head
        queue = queue.tail

        val toAdd = via collect { case Edge(v1, _, v2) if (v1 == n) && !(res contains v2) => v2 }

        queue = queue ++ toAdd
        res = res ++ toAdd
      }

      res
    }

    def escapingNodes: Set[Node] = {
      val from = (ptGraph.vertices.collect { case n: LNode => n; case n: PNode => n }) ++ eNodes + GBNode

      reachableNodes(from, Set() ++ iEdges)
    }

    def processLoad(vRef: CFG.Ref, vFrom: CFG.Ref, field: Field, pPoint: Int) = {
      val nodesFromVFrom = iEdges.collect { case Edge(v1,l,v2) if (l == field) && (getL(vFrom) contains v1) => v2 }
      val escapings      = escapingNodes intersect getL(vFrom)

      if (escapings.isEmpty) {
        // If nodes of vFrom are not escaping the current scope:
        copy(locState = locState + (vRef -> nodesFromVFrom))
      } else {
        val lNode = LNode(pPoint, false)
        addNode(lNode).setL(vRef, (nodesFromVFrom + lNode)).addOEdges(escapings, field, Set(lNode))
      }
    }

    def setL(ref: CFG.Ref, nodes: Set[Node]) = {
      copy(locState = locState + (ref -> nodes), isBottom = false)
    }

    def getL(ref: CFG.Ref): Set[Node] = locState(ref)

    def removeNode(node: Node) =
      copy(ptGraph = ptGraph - node, locState = locState.map{ case (ref, nodes) => ref -> nodes.filter(_ != node)}, eNodes = eNodes - node, rNodes = rNodes - node, isBottom = false)

    def addNode(node: Node) =
      copy(ptGraph = ptGraph + node, isBottom = false)

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

    def removeIEdgesFrom(lv1: Set[Node], field: Field) = {
      var iEdgesToRemove = iEdges filter (e => lv1.contains(e.v1) && e.label == field)
      var iEdgesNew = iEdges
      var newGraph = ptGraph
      for (e <- iEdgesToRemove) {
        newGraph -= e
        iEdgesNew -= e
      }
      copy(ptGraph = newGraph, iEdges = iEdgesNew, isBottom = false)
    }

    def addENodes(nodes: Set[Node]) = {
      copy(ptGraph = ptGraph ++ nodes, eNodes = eNodes ++ nodes, isBottom = false)
    }

    def setReturnNodes(ref: CFG.Ref) = {
      val nodes = getL(ref)
      copy(ptGraph = ptGraph ++ nodes, rNodes = nodes, isBottom = false)
    }

    def addGlobalNode() = {
      copy(ptGraph = ptGraph + GBNode, isBottom = false)
    }

    def duplicate = this
  }

  def isUniqueObjectNode(node: Node) = {
    node.unique // For now, only 'this' nodes are unique
  }

  object BottomPTEnv extends PTEnv(true)

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    class PointToTF(fun: AbsFunction) extends TransferFunctionAbs[PTEnv, CFG.Statement] {

      def apply(st: CFG.Statement, oldEnv: PTEnv): PTEnv = {
        var env = oldEnv

        def getNodesFromEnv(e: PTEnv)(sv: CFG.SimpleValue): Set[Node] = sv match {
          case r2: CFG.Ref => e.getL(r2)
          case n : CFG.Null => Set(NNode)
          case u : CFG.Unit => Set()
          case _ => Set(SNode)
        }

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

        type Transformer = (PTEnv, NodeMap) => (PTEnv, NodeMap)

        def getNodes(sv: CFG.SimpleValue) = getNodesFromEnv(env)(sv)

        // Merging graphs  of callees into the caller
        def interProc(eCaller: PTEnv, target: Symbol, call: CFG.AssignApplyMeth): PTEnv = {

          def p(env: PTEnv) = env.copy(locState = Map().withDefaultValue(Set()))
          def gc(env: PTEnv) = env // TODO
          def pi(env: PTEnv) = env // TODO

          def transFixPoint(envCallee: PTEnv, envInit: PTEnv, mapInit: NodeMap): (PTEnv, NodeMap) = {
            var env  = envInit
            var nmap = mapInit

            // Atomic transformers
            def gesc(n: Node) {
              env = env.copy(eNodes = env.eNodes ++ nmap(n))
            }

            def store(e: Edge) {
              val fromNodes = nmap(e.v1)
              if (fromNodes.forall(isUniqueObjectNode)) {
                // We try a strong update
                env = env.removeIEdgesFrom(fromNodes, e.label).addIEdges(fromNodes, e.label, nmap(e.v2))
              } else {
                // Fall back to weak update
                env = env.addIEdges(fromNodes, e.label, nmap(e.v2))
              }
            }

            def load(e: Edge) {
              val existingViaIEdge = nmap(e.v1) flatMap (n1 => (env.ptGraph.E collect { case oe if oe.v1 == n1 &&  oe.label == e.label => oe.v2})) 
              val newMap = nmap ++ (e.v2 -> existingViaIEdge)

              val a = nmap(e.v1) intersect env.escapingNodes

              if (a.isEmpty) {
                nmap = newMap
                if (existingViaIEdge.isEmpty) {
                  // Add oEdge
                  env = env.addOEdges(nmap(e.v1), e.label, Set(e.v2))
                  nmap = nmap + (e.v2 -> e.v2)
                }
              } else {
                env = env.addOEdges(a, e.label, Set(e.v2))
                nmap = newMap + (e.v2 -> e.v2)
              }
            }

            // Apply all transformers
            var lastEnv  = env
            var lastnmap = nmap

            var i = 0

            do {
              lastEnv  = env
              lastnmap = nmap

              i += 1


              for (eN <- envCallee.eNodes) {
                gesc(eN)
              }

              for (iE <- envCallee.iEdges) {
                store(iE)
              }

              for (oE <- envCallee.oEdges) {
                load(oE)
              }

            } while (lastEnv != env || lastnmap != nmap)


            (env, nmap)
          }

          def callerNodes(sv: CFG.SimpleValue) = getNodesFromEnv(eCaller)(sv)

          val oeCallee = getPTEnv(target)
          if (!oeCallee.isEmpty) {
            val eCallee = oeCallee.get

            val gcCallee = pi(gc(p(eCallee)))

            // Build map
            var map: NodeMap = NodeMap() ++ (PNode(0, true) -> callerNodes(call.obj)) + (GBNode -> GBNode) + (NNode -> NNode) + (SNode -> SNode)

            // Map all inside nodes to themselves
            map = map +++ eCallee.ptGraph.vertices.toSeq.collect{ case n: INode => (n: Node,Set[Node](n)) }
              
            funDecls.get(target) match {
              case Some(fun) => // Found the target function, we assign only object args to corresponding nodes
                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex if !isGroundClass(fun.CFGArgs(i).symbol)) {
                  map = map ++ (PNode(i+1, false) -> nodes)
                }

              case None => // Could not find the target fun declaration, we assign args as usual
                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex) {
                  map = map ++ (PNode(i+1, false) -> nodes) 
                }
            }

            val (newEnvTmp, newMap) = transFixPoint(gcCallee, eCaller, map)

            val newEnv = newEnvTmp.setL(call.r, gcCallee.rNodes flatMap newMap)

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
            afr.obj match {
              case sr: CFG.SymRef if sr.symbol.isModule =>
                // If we have r = obj.field where obj is a global object, we have that r is pointing to GLB
                env = env.addGlobalNode().setL(afr.r, Set(GBNode))
              case _ =>
                env = env.processLoad(afr.r, afr.obj, SymField(afr.field), afr.uniqueID)
            }
          case afw: CFG.AssignFieldWrite =>
            afw.obj match {
              case sr: CFG.SymRef if sr.symbol.isModule =>
                // If we do Obj.field = rhs, where Obj is a global object, rhs is escaping from the scope
                env = env.addENodes(getNodes(afw.rhs))
              case _ =>
                // Otherwise, we have obj.field = rhs
                val field = SymField(afw.field)
                val fromNodes = getNodes(afw.obj)

                if (fromNodes.forall(isUniqueObjectNode)) { // We are doing this.field = val, we can benefit from a strong update here
                  env = env.removeIEdgesFrom(fromNodes, field).addIEdges(fromNodes, field, getNodes(afw.rhs))
                } else {
                  env = env.addIEdges(fromNodes, field, getNodes(afw.rhs))
                }
            }

          case aam: CFG.AssignApplyMeth => // r = o.v(..args..)

            val (targets, optError) = fun.callTargets.get(aam) match {
              case Some((targets, exhaust)) if !exhaust && !settings.wholeCodeAnalysis =>
                (Set(), Some("targets are not exhaustive"))

              case Some((targets, exhaust)) =>
                val unanalyzable = targets.filter(t => getPTEnv(t).isEmpty)
                if (!unanalyzable.isEmpty) {
                  (Set(), Some("targets "+unanalyzable.mkString(", ")+" have no corresponding PT env"))
                } else {
                  (targets, None)
                }

              case _ =>
                (Set(), Some("no target symbol could be found"))
            }



            if (optError.isEmpty) {
              env = ((BottomPTEnv:PTEnv) /: targets) ((e, sym) => e union interProc(env, sym, aam))
            } else {
              settings.ifVerbose {
                reporter.warn("Aborted call analysis of "+aam+" because "+optError.get)
              }

              env = env.addGlobalNode().setL(aam.r, Set(GBNode))

              env = (env /: (aam.obj +: aam.args)) { (e, a) => e.addENodes(getNodes(a)) }
            }

          case an: CFG.AssignNew => // r = new A
            val iNodeUnique    = INode(an.uniqueID, true)
            val iNodeNotUnique = INode(an.uniqueID, false)
            if (env.ptGraph.V contains iNodeUnique) {
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

    def analyze(fun: AbsFunction) = {
      val cfg       = fun.cfg
      val bottomEnv = BottomPTEnv
      var baseEnv   = new PTEnv()

      settings.ifVerbose {
        reporter.info("Analyzing "+fun.symbol.fullName+"...")
      }


      // 1) We add 'this' and argument nodes
      val thisNode = PNode(0, true)
      baseEnv = baseEnv.addNode(thisNode).setL(cfg.thisRef, Set(thisNode))

      for ((a, i) <- fun.CFGArgs.zipWithIndex) {
        // If we are touching a ground class, we can safely make it point to a unique Scala Node
        val pNode = if (isGroundClass(a.symbol.tpe.typeSymbol)) {
          SNode
        } else {
          PNode(i+1, false)
        }
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
      val aa = new DataFlowAnalysis[PTEnv, CFG.Statement](bottomEnv, baseEnv, settings)

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
      predefinedPTClasses += definitions.ObjectClass -> BottomPTEnv

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

          val dest = name+"-pt.dot"

          reporter.info("Dumping Point-To Graph to "+dest+"...")
          new PTDotConverter(newGraph, "Point-to: "+name, e.rNodes, e.eNodes).writeFile(dest)
        }
      }
    }
  }
}

