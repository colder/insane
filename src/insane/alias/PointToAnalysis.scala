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

  case class PTEnv(ptGraph: PointToGraph,
                 locState: Map[CFG.Ref, Set[Node]],
                 iEdges: Set[IEdge],
                 oEdges: Set[OEdge],
                 rNodes: Set[Node],
                 isBottom: Boolean) extends dataflow.EnvAbs[PTEnv, CFG.Statement] {

    def this(isBottom: Boolean = false) = this(new PointToGraph(), Map().withDefaultValue(Set()), Set(), Set(), Set(), isBottom)

    /*
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
    */

    def getAllTargetsUsing(edges: Traversable[Edge])(from: Set[Node], via: Field): Set[Node] = {
      edges.collect{ case Edge(v1, f, v2) if (from contains v1) && (f == via) => v2 }.toSet
    }

    val getAllTargets   = getAllTargetsUsing(ptGraph.E)_
    val getWriteTargets = getAllTargetsUsing(iEdges)_
    val getReadTargets  = getAllTargetsUsing(oEdges)_

    /*
    def escapingNodes: Set[Node] = {
      val from = (ptGraph.vertices.collect { case n: LNode => n; case n: PNode => n }) ++ eNodes + GBNode

      reachableNodes(from, Set() ++ iEdges)
    }
    */

    /*
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
    */

    def setL(ref: CFG.Ref, nodes: Set[Node]) = {
      copy(locState = locState + (ref -> nodes), isBottom = false)
    }

    def getL(ref: CFG.Ref): Set[Node] = locState(ref)

    def removeNode(node: Node) =
      copy(ptGraph = ptGraph - node, locState = locState.map{ case (ref, nodes) => ref -> nodes.filter(_ != node)}, rNodes = rNodes - node, isBottom = false)

    def addNode(node: Node) =
      copy(ptGraph = ptGraph + node, isBottom = false)

    /**
     * Corresponds to:
     *   to = {..from..}.field @UniqueID
     */
    def read(from: Set[Node], field: Field, to: CFG.Ref, uniqueID: UniqueID) = {

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
          val lNode = LNode(CompoundUniqueID(uniqueID, ObjUniqueID(node)), false)
          res = res.addNode(lNode).addOEdges(Set(node), field, Set(lNode))
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
    def write(from: Set[Node], field: Field, to: Set[Node], uniqueID: UniqueID) = {
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
          val previouslyReachable = getAllTargets(Set(node), field)

          if (previouslyReachable.isEmpty) {
            // We need to add the artificial load node, as it represents the old state
            val lNode = LNode(CompoundUniqueID(uniqueID, ObjUniqueID(node)), false)

            newEnv = newEnv.addNode(lNode).addOEdges(Set(node), field, Set(lNode)).addIEdges(Set(node), field, Set(lNode))
          }

          // 2) We link that to node via a write edge
          newEnv.addIEdges(Set(node), field, previouslyReachable ++ to)
        }
      }

      newEnv
    }

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

    def removeIEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = iEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), iEdges = iEdges -- toRemove, isBottom = false)
    }

    def removeOEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = oEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), oEdges = oEdges -- toRemove, isBottom = false)
    }
      /*
      var newGraph = ptGraph
      var iEdgesNew = iEdges

      val isStrong = lv1.forall(_.unique) && (lv1.size*lv2.size == 1)
      */

      /*
      for (from <- lv1) {
        // Take existing edges from 'from'
        val existingEdges          = iEdges filter (e => e.v1 == from && e.label == field) toSeq

        // Divide existing edges' weight to fit the remaining weight
        val existingEdgesNewWeight = existingEdges map (e => e.copy(weight = e.weight * remaining))

        for ((eOld, eNew) <- existingEdges zip existingEdgesNewWeight) {
          // Remove the old pair of edges with old weight
          newGraph  -= eOld
          iEdgesNew -= eOld

          if (eNew.weight != 0) {
            // If the old edge has non-zero new weight, add new one back
            newGraph  += eNew
            iEdgesNew += eNew
          }
        }
      }

      // Add new edges as well, with new weight
      for (v1 <- lv1; v2 <- lv2) {
        val e = IEdge(v1, field, eWeight, v2)
        newGraph  += e
        iEdgesNew += e
      }
      */

/*
      copy(ptGraph = newGraph, iEdges = iEdgesNew, isBottom = false)
    }
    */

    /*
    def addENodes(nodes: Set[Node]) = {
      copy(ptGraph = ptGraph ++ nodes, eNodes = eNodes ++ nodes, isBottom = false)
    }
    */

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
         * write node is not present in the others, in that case, it consists of a
         * weak update in the resulting env
         */


        var newIEdges = envs.flatMap(_.iEdges).toSet
        var newOEdges = envs.flatMap(_.oEdges).toSet

        val newGraph = (envs.map(_.ptGraph).reduceLeft(_ union _)).copy(edges = Set[Edge]() ++ newOEdges ++ newIEdges)

        new PTEnv(
          newGraph,
          envs.flatMap(_.locState.keySet).toSet.map((k: CFG.Ref) => k -> (envs.map(e => e.locState(k)).reduceRight(_ ++ _))).toMap.withDefaultValue(Set()),
          newIEdges,
          newOEdges,
          envs.flatMap(_.rNodes).toSet,
          false)
        /*

        val n = envs.size

        var edgesMap = Map[(Node, Node, Field), Set[Weight]]().withDefaultValue(Set())

        for (e <- envs.flatMap(_.iEdges.toSeq)) {
          val key = (e.v1, e.v2, e.label)

          edgesMap += key -> (edgesMap(key) + e.weight * Fraction(1, n))
        }

        val newIEdges = edgesMap map { case ((v1, v2, l), ws) => IEdge(v1, l, ws.reduceLeft(_ + _) ,v2) } toSet

        val newGraph = (envs.map(_.ptGraph).reduceLeft(_ union _)).copy(edges = Set[Edge]() ++ envs.flatMap(_.oEdges) ++ newIEdges)

      */
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

        type Transformer = (PTEnv, NodeMap) => (PTEnv, NodeMap)

        def getNodes(sv: CFG.SimpleValue) = getNodesFromEnv(env)(sv)

        // Merging graphs  of callees into the caller
        def interProc(eCaller: PTEnv, target: Symbol, call: CFG.AssignApplyMeth): PTEnv = {

          /*
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

              val isStrong = fromNodes.forall(_.isSingleton)

              env = env.addIEdges(fromNodes, e.label, nmap(e.v2))
            }

            def load(e: Edge) {
              val existingViaIEdge = nmap(e.v1) flatMap (n1 => (env.ptGraph.E collect { case oe if oe.v1 == n1 && oe.label == e.label => oe.v2})) 
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
            var map: NodeMap = NodeMap() ++ (PNode(0) -> callerNodes(call.obj)) + (GBNode -> GBNode) + (NNode -> NNode) + (SNode -> SNode)

            // Map all inside nodes to themselves
            map = map +++ eCallee.ptGraph.vertices.toSeq.collect{ case n: INode => (n: Node,Set[Node](INode(n.pPoint, false))) }
              
            funDecls.get(target) match {
              case Some(fun) => // Found the target function, we assign only object args to corresponding nodes
                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex if !isGroundClass(fun.CFGArgs(i).symbol)) {
                  map = map ++ (PNode(i+1) -> nodes)
                }

              case None => // Could not find the target fun declaration, we assign args as usual
                for (((a, nodes),i) <- call.args.map(a => (a, callerNodes(a))).zipWithIndex) {
                  map = map ++ (PNode(i+1) -> nodes) 
                }
            }

            val (newEnvTmp, newMap) = transFixPoint(gcCallee, eCaller, map)

            val newEnv = newEnvTmp.setL(call.r, gcCallee.rNodes flatMap newMap)

            newEnv
          } else {
            reporter.error("Unknown env for target "+target+" for call: "+call)
            eCaller
          }
        */
          eCaller
        }

        st match {
          case av: CFG.AssignVal =>
            env = env.setL(av.r, getNodes(av.v))

          case afr: CFG.AssignFieldRead =>
            val field = SymField(afr.field)

            val fromNodes: Set[Node] = afr.obj match {
              case sr: CFG.SymRef if sr.symbol.isModule =>
                env = env.addGlobalNode()
                Set(GBNode)
              case _ =>
                getNodes(afr.obj)
            }

            env = env.read(fromNodes, field, afr.r, IntUniqueID(afr.uniqueID))

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

            env = env.write(fromNodes, field, getNodes(afw.rhs), IntUniqueID(afw.uniqueID))

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
              if (targets.isEmpty) {
                env = BottomPTEnv
              } else {
                env = PointToLattice.join(targets map (sym => interProc(env, sym, aam)) toSeq : _*)
              }
            } else {
              settings.ifVerbose {
                reporter.warn("Aborted call analysis of "+aam+" because "+optError.get)
              }

              env = env.addGlobalNode().setL(aam.r, Set(GBNode))

              /*
              env = (env /: (aam.obj +: aam.args)) { (e, a) => e.addENodes(getNodes(a)) }
              */
            }

          case an: CFG.AssignNew => // r = new A
            val iNodeUnique    = INode(IntUniqueID(an.uniqueID), true)
            val iNodeNotUnique = INode(IntUniqueID(an.uniqueID), false)
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

    def analyze(fun: AbsFunction) = {
      val cfg       = fun.cfg
      var baseEnv   = new PTEnv()

      settings.ifVerbose {
        reporter.info("Analyzing "+fun.uniqueName+"...")
      }


      // 1) We add 'this' and argument nodes
      val thisNode = PNode(0)

      baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

      for ((a, i) <- fun.CFGArgs.zipWithIndex) {
        // If we are touching a ground class, we can safely make it point to a unique Scala Node
        val pNode = if (isGroundClass(a.symbol.tpe.typeSymbol)) {
          SNode
        } else {
          PNode(i+1)
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

          val dest = name+"-pt.dot"

          reporter.info("Dumping Point-To Graph to "+dest+"...")
          new PTDotConverter(newGraph, "Point-to: "+name, e.rNodes).writeFile(dest)
        }
      }
    }
  }
}

