package insane
package alias

import utils._
import analysis._
import CFG.ControlFlowGraph

trait PointToAnalysis extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    case class Env(ptGraph: PointToGraph,
                   locState: Map[CFG.Ref, Set[Node]],
                   iEdges: Set[IEdge],
                   oEdges: Set[OEdge],
                   eNodes: Set[Node],
                   rNodes: Set[Node],
                   isBottom: Boolean) extends DataFlowEnvAbs[Env, CFG.Statement] {

      def this(isBottom: Boolean = false) = this(new PointToGraph(), Map().withDefaultValue(Set()), Set(), Set(), Set(), Set(), isBottom)

      def union(that: Env) = {
        Env(ptGraph union that.ptGraph,
            (locState.keySet ++ that.locState.keySet).map(k => k -> (locState(k)++that.locState(k))).toMap,
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
        val from = (ptGraph.vertices.collect { case n: LNode => n; case n: PNode => n }) ++ eNodes ++ rNodes + GBNode

        reachableNodes(from, Set() ++ iEdges)
      }

      def processLoad(vRef: CFG.Ref, vFrom: CFG.Ref, field: Field, pPoint: Int) = {
        val nodesFromVFrom = iEdges.collect { case Edge(v1,l,v2) if (l == field) && (getL(vFrom) contains v1) => v2 }
        val escapings      = escapingNodes intersect getL(vFrom)

        if (escapings.isEmpty) {
          // If nodes of vFrom are not escaping the current scope:
          copy(locState = locState + (vRef -> nodesFromVFrom))
        } else {
          val lNode = LNode(pPoint)
          addNode(lNode).setL(vRef, (nodesFromVFrom + lNode)).addOEdges(escapings, field, Set(lNode))
        }
      }

      def setL(ref: CFG.Ref, nodes: Set[Node]): Env = {
        copy(locState = locState + (ref -> nodes), isBottom = false)
      }

      def getL(ref: CFG.Ref): Set[Node] = locState(ref)

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

      def addENodes(nodes: Set[Node]) = {
        copy(ptGraph = ptGraph ++ nodes, eNodes = eNodes ++ nodes, isBottom = false)
      }

      def setRNodes(nodes: Set[Node]) = {
        copy(ptGraph = ptGraph ++ nodes, rNodes = nodes, isBottom = false)
      }

      def addGlobalNode: Env = {
        copy(ptGraph = ptGraph + GBNode, isBottom = false)
      }

      def duplicate = this
    }

    object BottomEnv extends Env(true)

    class PointToTF(cfg: FunctionCFG) extends TransferFunctionAbs[Env, CFG.Statement] {

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        var env = oldEnv

        def getNodes(sv: CFG.SimpleValue): Set[Node] = sv match {
          case r2: CFG.Ref => env.getL(r2)
          case n : CFG.Null => Set()
          case _ => Set()
        }

        st match {
          case av: CFG.AssignVal =>
            if (av.r == cfg.retval) {
              println("Found return!")
              env = env.setRNodes(getNodes(av.v))
            } else {
              env = env.setL(av.r, getNodes(av.v))
            }

          case afr: CFG.AssignFieldRead =>
            afr.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we have r = obj.field where obj is a global object, we have that r is pointing to GLB
                env = env.addGlobalNode.setL(afr.r, Set(GBNode))
              case _ =>
                env = env.processLoad(afr.r, afr.obj, SymField(afr.field), afr.uniqueID)
            }
          case afw: CFG.AssignFieldWrite =>
            afw.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we do Obj.field = rhs, where Obj is a global object, rhs is escaping from the scope
                env = env.addENodes(getNodes(afw.rhs))
              case _ =>
                // Otherwise, we have obj.field = rhs
                env = env.addIEdges(getNodes(afw.obj), SymField(afw.field), getNodes(afw.rhs))
            }

          case aam: CFG.AssignApplyMeth => // r = o.v(..args..)

          case an: CFG.AssignNew => // r = new A(.. args ..)
            val iNode = INode(an.uniqueID)
            env = env.addNode(iNode).setL(an.r, Set(iNode))
            // We need to call the constructor as well

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

    def analyze(fun: AbsFunction) {
      val cfg       = fun.cfg.get
      val bottomEnv = BottomEnv;
      var baseEnv   = new Env();

      // 1) We add 'this' and argument nodes
      for ((a, i) <- (cfg.thisReferences.toSeq ++ fun.CFGArgs).zipWithIndex) {
        val pNode = PNode(i)
        baseEnv = baseEnv.addNode(pNode).setL(a, Set(pNode))
      }


      // 2) We run a fix-point on the CFG
      val ttf = new PointToTF(cfg)
      val aa = new DataFlowAnalysis[Env, CFG.Statement](bottomEnv, baseEnv, settings)

      reporter.info("Analyzing "+fun.symbol.fullName+"...")

      aa.computeFixpoint(cfg, ttf)

      // 3) We retrieve the exit CFG
      val e = aa.getResult(cfg.exit)

      val name = fun.symbol.fullName;
      if (settings.dumpPTGraph(name)) {
        println(e)
        var newGraph = e.ptGraph

        // 4) We complete the graph with local vars -> nodes association, for clarity
        for ((ref, nodes) <- e.locState; n <- nodes) {
          newGraph += VEdge(VNode(ref), n)
        }

        val dest = name+"-pt.dot"

        reporter.info("Dumping Point-To Graph to "+dest+"...")
        new PTDotConverter(newGraph, "Point-to: "+name).writeFile(dest)
      }
    }

    def run {
      for ((sym, f) <- funDecls) {
        analyze(f)
      }
    }
  }
}

