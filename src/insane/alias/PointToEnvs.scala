package insane
package alias

import utils._
import utils.Reporters._
import CFG._

trait PointToEnvs extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  var diffCNT = 1;

  case class PTEnv(ptGraph: PointToGraph,
                 locState: Map[CFG.Ref, Set[Node]],
                 iEdges: Set[IEdge],
                 oEdges: Set[OEdge],
                 danglingCalls: Map[CFG.AssignApplyMeth, String],
                 isBottom: Boolean,
                 isEmpty: Boolean) extends dataflow.EnvAbs[PTEnv] {

    def asPartialEnv(danglingCalls: Map[CFG.AssignApplyMeth, String]) = {
      PTEnv(new PointToGraph(),
           Map().withDefaultValue(Set()),
           Set(),
           Set(),
           danglingCalls,
           false,
           false)

    }

    def this(isBottom: Boolean = false, isEmpty: Boolean = false) =
      this(new PointToGraph(),
           Map().withDefaultValue(Set()),
           Set(),
           Set(),
           Map(),
           isBottom,
           isEmpty)

    val isPartial = !danglingCalls.isEmpty

    def getAllTargetsUsing(edges: Traversable[Edge])(from: Set[Node], via: Field): Set[Node] = {
      edges.collect{ case Edge(v1, f, v2) if (from contains v1) && (f == via) => v2 }.toSet
    }

    val getAllTargets   = getAllTargetsUsing(ptGraph.E)_
    val getWriteTargets = getAllTargetsUsing(iEdges)_
    val getReadTargets  = getAllTargetsUsing(oEdges)_

    def diffWith(that: PTEnv) = {
      def setDiff[T](a: Set[T], b: Set[T]) {
        for (ae <- a -- b) {
          println(" - "+ae)
        }
        for (be <- b -- a) {
          println(" + "+be)
        }
      }

      def mapDiff[T,U](a: Map[T,U], b: Map[T,U]) {
        for (k <- a.keySet ++ b.keySet) {
          if (a.isDefinedAt(k) && b.isDefinedAt(k)) {
            if (a(k) != b(k)) {
              println(" - "+k+" -> "+a(k))
              println(" + "+k+" -> "+b(k))
            }
          } else if (a.isDefinedAt(k)) {
              println(" - "+k+" -> "+a(k))
          } else if (b.isDefinedAt(k)) {
              println(" + "+k+" -> "+b(k))
          }
        }
      }
      println("LocState:")
      mapDiff(this.locState, that.locState)

      println("Nodes:")
      setDiff(this.ptGraph.V, that.ptGraph.V)

      println("Edges:")
      setDiff(this.ptGraph.E, that.ptGraph.E)

      if (this.isPartial != that.isPartial) {
        println("isPartial differs: "+this.isPartial+" -> "+that.isPartial);
      }
      if (this.isBottom != that.isBottom) {
        println("isBottom differs: "+this.isBottom+" -> "+that.isBottom);
      }

      println("Debug graph dumped to diff-"+diffCNT+"-*")

      dumpDiff(this, that, "diff-"+diffCNT+".dot")

      diffCNT += 1
    }

    def setL(ref: CFG.Ref, nodes: Set[Node]) = {
      copy(locState = locState + (ref -> nodes), ptGraph = ptGraph ++ nodes, isBottom = false)
    }

    def getL(ref: CFG.Ref, readOnly: Boolean): (PTEnv, Set[Node]) = {
      if (locState contains ref) {
        (this, locState(ref))
      } else {
        if (readOnly) {
          reporter.error("Consistency problem: local field accessed without associated nodes in a partial-graph while in read-only context");
          (this, locState(ref))
        } else {
          /**
           * XXX FIXME:
           * 
           * Type signature should be available here, and it should allow us to
           * lookup whether this ref corresponds to an arg for which we have a
           * precise type signature. Falling back to declaration type means
           * imprecision.
           */
          val n = ref match {
            case CFG.ObjRef(sym, tpe) =>
              OBNode(sym)

            case _ =>
              val sig = SigEntry.fromTypeInfo(TypeInfo.subtypeOf(ref.tpe));
              LVNode(ref, sig)
          }

          (addNode(n).setL(ref, Set(n)), Set(n))
        }
      }
    }

    def splitNode(from: Node, to: Node) = {
      if (from == to) {
        this
      } else if (ptGraph.V contains to) {
        this
      } else {

        var newEnv = copy(ptGraph = ptGraph + to)

        // Update iEdges
        for (iEdge @ IEdge(v1, lab, v2) <- iEdges if v1 == from || v2 == from) {
          val newIEdge = IEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)

          newEnv = newEnv.copy(ptGraph = newEnv.ptGraph + newIEdge, iEdges = newEnv.iEdges + newIEdge)
        }


        // Update oEdges
        for (oEdge @ OEdge(v1, lab, v2) <- oEdges if v1 == from || v2 == from) {
          val newOEdge = OEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)

          newEnv = newEnv.copy(ptGraph = newEnv.ptGraph + newOEdge, oEdges = newEnv.oEdges + newOEdge)
        }


        // Update locState
        newEnv = newEnv.copy(locState = newEnv.locState.map{ case (ref, nodes) => ref -> (if (nodes contains from) nodes + to else nodes) }.withDefaultValue(Set()))

        newEnv
      }
    }

    def replaceNode(from: Node, toNodes: Set[Node]) = {
      assert(!(toNodes contains from), "Recursively replacing "+from+" with "+toNodes.mkString("{", ", ", "}")+"!")

      var newEnv = copy(ptGraph = ptGraph - from ++ toNodes, isBottom = false)

      // Update iEdges
      for (iEdge @ IEdge(v1, lab, v2) <- iEdges if v1 == from || v2 == from; to <- toNodes) {
        val newIEdge = IEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)

        newEnv = newEnv.copy(ptGraph = newEnv.ptGraph - iEdge + newIEdge, iEdges = newEnv.iEdges - iEdge + newIEdge)
      }


      // Update oEdges
      for (oEdge @ OEdge(v1, lab, v2) <- oEdges if v1 == from || v2 == from; to <- toNodes) {
        val newOEdge = OEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)

        newEnv = newEnv.copy(ptGraph = newEnv.ptGraph - oEdge + newOEdge, oEdges = newEnv.oEdges - oEdge + newOEdge)
      }


      // Update locState
      newEnv = newEnv.copy(locState = newEnv.locState.map{ case (ref, nodes) => ref -> (if (nodes contains from) nodes - from ++ toNodes else nodes) }.withDefaultValue(Set()))

      newEnv
    }

    def mergeSimilarNodes = {
      /**
       * Merging similar LNodes, that is, nodes that have the same origin, the same field, and the same type
       */

      val groupedLNodes = oEdges.collect{
          case OEdge(v1, lab, v2: LNode) => (v1, lab, v2)
        }.groupBy{
          case (v1, lab, v2) => (v1, lab, v2.types)
        }.collect{
          case (k, es) if es.size > 1 => es.map(_._3).toList.sortBy(_.pPoint)
        }

      var res = this

      for (lnodes <- groupedLNodes) {
        res = res.mergeNodes(lnodes)
      }

      res
    }
   
    def mergeNodes(nodes: Traversable[Node]) = {
      assert(nodes.size > 1, "Merging a single node ?!?")

      val mergeInto = nodes.head
      val mergeFrom = nodes.tail.toSet

      var newEnv = copy(ptGraph = ptGraph -- mergeFrom, isBottom = false)

      // Update iEdges
      for (iEdge @ IEdge(v1, lab, v2) <- iEdges if mergeFrom(v1) || mergeFrom(v2)) {
        val newIEdge = IEdge(if (mergeFrom(v1)) mergeInto else v1, lab, if (mergeFrom(v2)) mergeInto else v2)

        newEnv = newEnv.copy(ptGraph = newEnv.ptGraph - iEdge + newIEdge, iEdges = newEnv.iEdges - iEdge + newIEdge)
      }


      // Update oEdges
      for (oEdge @ OEdge(v1, lab, v2) <- oEdges if mergeFrom(v1) || mergeFrom(v2)) {
        val newOEdge = OEdge(if (mergeFrom(v1)) mergeInto else v1, lab, if (mergeFrom(v2)) mergeInto else v2)

        newEnv = newEnv.copy(ptGraph = newEnv.ptGraph - oEdge + newOEdge, oEdges = newEnv.oEdges - oEdge + newOEdge)
      }


      // Update locState
      newEnv = newEnv.copy(locState = newEnv.locState.map{ case (ref, nodes) => ref -> (if (nodes exists mergeFrom) nodes -- mergeFrom + mergeInto else nodes) }.withDefaultValue(Set()))

      newEnv
    }

    def addNode(node: Node) =
      copy(ptGraph = ptGraph + node, isBottom = false, isEmpty = false)

    lazy val loadNodes: Set[LNode] = {
      ptGraph.V.collect { case l: LNode => l }
    }

    def getWriteOrElseReadTargets(nodes: Set[Node], field: Field): Set[Node] = {
      val writeTargets = getWriteTargets(nodes, field)

      if (writeTargets.isEmpty) {
        getReadTargets(nodes, field)
      } else {
        writeTargets
      }
    }

    /**
     * Corresponds to:
     *   to = {..from..}.field @UniqueID
     */
    def read(from: Set[Node], field: Field, to: CFG.Ref, uniqueID: UniqueID) = {

      var res = this

      var pointResults = Set[Node]()

      for (tmpNode <- from) {

        val node = tmpNode match {
          case ln @ LNode(from, via, pPoint, types) =>
            ln
          case n =>
            n
        }

        val pointed = getWriteOrElseReadTargets(Set(node), field);

        if (pointed.isEmpty) {
          safeLNode(node, field, uniqueID) match {
            case Some(lNode) =>
              res = res.addNode(lNode).addOEdge(node, field, lNode)
              pointResults += lNode
            case None =>
              println("@@@@  "+node+" via "+field +" ( "+node.types+" )( "+node.sig+" )")
              println("###################\n"+node.types.tpe.decls)
              println("###################\n"+node.types.tpe.typeSymbol)
              debugSymbol(node.types.tpe.typeSymbol)
              dumpPTE(this, "error.dot");
              dumpAnalysisStack()
              reporter.fatal("Unable to create LNode for read from "+node+" via "+field);
          }
        } else {
          pointResults ++= pointed
        }
      }

      settings.ifDebug {
        if (pointResults.isEmpty) {
    //      reporter.debug("Unable to read ("+from.mkString(" | ")+")."+field)
        } else {
    //      reporter.debug("("+from.map(f => f+"["+f.types+"]").mkString(" | ")+")."+field+" = "+pointResults)
        }
      }

      res.setL(to, pointResults)
    }

    /**
     * Corresponds to:
     *   {..from..}.field = {..to..} @UniqueID
     */
    def write(from: Set[Node], field: Field, to: Set[Node], allowStrongUpdates: Boolean) = {
      if (from.size == 0) {
        reporter.error("Writing with an empty {..from..} set!")
      }

      if (to.size == 0) {
        reporter.error("Writing with an empty {..to..} set!")
      }

      var newEnv = this

      val isStrong = from.forall(_.isSingleton) && from.size == 1 && allowStrongUpdates

      if (isStrong) {
        // If strong update:

        val writeTargets = getWriteTargets(from, field)
        // 1) We remove all previous write edges
        newEnv = newEnv.removeIEdges(from, field, writeTargets)

        // 2) We add back only the new write edge
        newEnv = newEnv.addIEdges(from, field, to)

        val oldReadsSet = newEnv.getReadTargets(from, field)

        // 3)
        // If the graph contains a load node on the same field, we need to keep
        // intermediate values, to be conservative

        // We need to check if the load node we detected was not the
        // previously read value on the same object, in that case no need to
        // add an OEdge
        if (newEnv.ptGraph.V.exists { case ln @ LNode(_, f, _, _) if f == field && !oldReadsSet(ln) => true; case _ => false }) {
          newEnv = newEnv.addOEdges(from, field, writeTargets)
        }
      } else {
        // If weak update:

        // For each actual source node:
        for (node <- from) {
          // 1) We check for an old node reachable
          val previouslyPointed = getWriteOrElseReadTargets(Set(node), field);

          if (previouslyPointed.isEmpty) {
            node match {
              case i: INode =>
                /**
                 * This can only occur when fix-pointing, we do not introduce a
                 * load node here as the old value is already defined in future
                 * writes
                 */
              case _ =>
                // We need to add the artificial load node, as it represents the old state
                safeLNode(node, field, new UniqueID(0)) match {
                  case Some(lNode) =>
                    //for (nodeToAdd <- findSimilarLNodes(lNode, newEnv.ptGraph.V)) {
                    //  newEnv = newEnv.addNode(nodeToAdd).addOEdge(node, field, nodeToAdd).addIEdge(node, field, nodeToAdd)
                    //}
                    newEnv = newEnv.addNode(lNode).addOEdge(node, field, lNode).addIEdge(node, field, lNode)
                  case None =>
                    //reporter.error("Unable to create LNode for write from "+node+" via "+field)
                    //sys.error("bleh")
                }
            }
          }

          // 2) We link that to node via a write edge
          newEnv = newEnv.addIEdges(Set(node), field, previouslyPointed ++ to)
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
      copy(ptGraph = newGraph, oEdges = oEdgesNew, isBottom = false, isEmpty = false)
    }

    def hasIEdge(v1: Node, field: Field, v2: Node) = {
      iEdges.exists {
        case IEdge(i1, ifield, i2) if (i1, ifield, i2) == (v1, field, v2) =>
          true 
        case _ =>
          false
      }
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
      copy(ptGraph = newGraph, iEdges = iEdgesNew, isBottom = false, isEmpty = false)
    }

    def removeIEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = iEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), iEdges = iEdges -- toRemove, isBottom = false)
    }

    def removeOEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
      val toRemove = oEdges.filter(e => lv1.contains(e.v1) && lv2.contains(e.v2) && e.label == field)

      copy(ptGraph = (ptGraph /: toRemove) (_ - _), oEdges = oEdges -- toRemove, isBottom = false)
    }

    def addGlobalNode() = {
      copy(ptGraph = ptGraph + GBNode, isBottom = false, isEmpty = false)
    }

    // def modifiesClause: ModifyClause = {
    //   import scala.collection.mutable.Stack

    //   /**
    //    * Check if there is any reachable IEdge from
    //    * 1) Params
    //    * 2) Global Objects
    //    **/

    //   var seen    = Set[Node]()
    //   var effects = Set[ModifyClauseEffect]()

    //   for (n <- ptGraph.V) n match {
    //     // At this point, remaining LVNodes are parameters
    //     case _: LVNode | _: GloballyReachableNode =>
    //       visitRoot(n)
    //     case _ =>
    //   }

    //   def visitRoot(n: Node) {
    //     def visit(n: Node, root: Node, path: List[Field]) {

    //       seen += n

    //       for (e @ Edge(v1, via, v2) <- ptGraph.outEdges(n)) {
    //         val newPath = via :: path

    //         e match {
    //           case _: IEdge =>
    //             effects += ModifyClauseEffect(newPath.reverse, root)
    //           case _ =>
    //         }

    //         if (!seen(v2)) {
    //           visit(v2, root, newPath)
    //         }
    //       }
    //     }

    //     visit(n, n, Nil)
    //   }

    //   ModifyClause(effects)
    // }

    def duplicate = this

    def getNodes(sv: CFG.SimpleValue, readonly: Boolean = false): (PTEnv, Set[Node]) = sv match {
      case r2: CFG.Ref          => getL(r2, readonly)
      case n : CFG.Null         => (this, Set(NNode))
      case u : CFG.Unit         => (this, Set(UNode))
      case _: CFG.StringLit     => (this, Set(StringLitNode))
      case _: CFG.AnyStringLit  => (this, Set(StringLitNode))
      case b: CFG.BooleanLit    => (this, Set(BooleanLitNode(b.v)))
      case _: CFG.AnyBooleanLit => (this, Set(AnyBooleanLitNode))
      case v: CFG.LongLit       => (this, Set(LongLitNode(v.v)))
      case _: CFG.AnyLongLit    => (this, Set(AnyLongLitNode))
      case v: CFG.IntLit        => (this, Set(IntLitNode(v.v)))
      case _: CFG.AnyIntLit     => (this, Set(AnyIntLitNode))
      case _: CFG.CharLit       => (this, Set(CharLitNode))
      case _: CFG.AnyCharLit    => (this, Set(CharLitNode))
      case _: CFG.ByteLit       => (this, Set(ByteLitNode))
      case _: CFG.AnyByteLit    => (this, Set(ByteLitNode))
      case _: CFG.FloatLit      => (this, Set(FloatLitNode))
      case _: CFG.AnyFloatLit   => (this, Set(FloatLitNode))
      case _: CFG.DoubleLit     => (this, Set(DoubleLitNode))
      case _: CFG.AnyDoubleLit  => (this, Set(DoubleLitNode))
      case _: CFG.ShortLit      => (this, Set(ShortLitNode))
      case _: CFG.AnyShortLit   => (this, Set(ShortLitNode))
    }

    def cleanLocState(): PTEnv = {
      copy(locState = Map().withDefaultValue(Set()))
    }

    def cleanLocState(fun: FunctionCFG): PTEnv = {
      // We remove locstate assignments for complete (non-partial graphs) other
      // than for args, this, or retval other should never be needed
      copy(locState = locState filter {
        case (r, nodes) =>
          val kind = r match {
            case tr: CFG.ThisRef =>
              true
            case r =>
              fun.args contains r
          }

          kind || (r == fun.retval)
        })
    }

    def cleanUnreachableForSummary(fun: FunctionCFG): PTEnv = {
      // We want to remove any node, edge, that is not reachable
      // Perform DFS on the graph from every reachable nodes, mark nodes and
      // edges, remove the rest
      cleanUnreachableStartingFrom(
        Set[Node]() ++ ((fun.args++Set(fun.mainThisRef, fun.retval)) flatMap locState) ++
          ptGraph.V.filter(_.isInstanceOf[GloballyReachableNode])
        , aggressive = true
      )
    }

    def cleanUnreachableForPartial(): PTEnv = {
      cleanUnreachableStartingFrom(
        Set[Node]() ++ locState.flatMap(_._2) ++
          ptGraph.V.filter(v => v.isInstanceOf[GloballyReachableNode] || v.isInstanceOf[LVNode])
          // Are we sure we need to include LVNodes on top of locState.values?
        , aggressive = false
      )
    }

    def cleanUnreachableStartingFrom(initMarkedNodes: Traversable[Node], aggressive: Boolean = false): PTEnv = {
      var markedNodes = Set[Node]() ++ initMarkedNodes

      if (aggressive) {
        // Aggressive backward search of write effects
        // We generally discard read effects here if they do not lead us to
        // either important nodes or write effects.
        def traverseNodeBackward(n: Node, thenreachable : Set[Node]): Unit = {
          for (e <- ptGraph.inEdges(n) if !thenreachable(e.v1)) {
            if (markedNodes(e.v1)) {
              markedNodes = markedNodes ++ thenreachable + n
              return;
            } else {
              traverseNodeBackward(e.v1, thenreachable + n)
            }
          }
        }

        for (ie <- ptGraph.E.filter(_.isInstanceOf[IEdge])) {
          traverseNodeBackward(ie.v1, Set(ie.v2))
        }
      } else {
        // Simple forward DFS of read+write effects
        var queue            = markedNodes.toList

        while (!queue.isEmpty) {
          val n = queue.head
          queue = queue.tail

          for (e <- ptGraph.outEdges(n) if !(markedNodes contains e.v2)) {
            markedNodes += e.v2

            queue = e.v2 :: queue
          }
        }
      }

      // We only keep edges between reachable nodes
      val markedEdges = ptGraph.E.filter(e => markedNodes(e.v1) && markedNodes(e.v2))

      new PTEnv(new PointToGraph(markedNodes, markedEdges),
                locState,
                markedEdges.collect{ case e: IEdge => e },
                markedEdges.collect{ case e: OEdge => e },
                danglingCalls,
                isBottom,
                isEmpty);
    }

    def cleanIsolatedVertices() : PTEnv = {
      val allLocState = locState.flatMap(_._2).toSet

      val nodes = ptGraph.V.filter{ n =>
        !ptGraph.outEdges(n).isEmpty  || !ptGraph.inEdges(n).isEmpty || allLocState(n)
      }

      new PTEnv(new PointToGraph(nodes, ptGraph.E),
                locState,
                iEdges,
                oEdges,
                danglingCalls,
                isBottom,
                isEmpty);

    }


    def cleanExtraLoadEdges(): PTEnv = {
      val fieldsLoaded = ptGraph.V.collect{ case LNode(_, via, _, _) => via }.toSet

      val edgesToRemove = oEdges.filter(e => !fieldsLoaded(e.label))

      if (edgesToRemove.isEmpty) {
        this
      } else {
        settings.ifDebug {
          reporter.debug("Found "+edgesToRemove.size+" edges to remove!")
        }

        new PTEnv(new PointToGraph(ptGraph.V, ptGraph.E -- edgesToRemove),
                  locState,
                  iEdges,
                  oEdges -- edgesToRemove,
                  danglingCalls,
                  isBottom,
                  isEmpty);
        
      }
    }

    def collapseDuplicatedNodes(): PTEnv = {
      case class DupNodeID(tpe: TypeInfo, kind: Int, edgesInfo: Set[(Int, Field, Node)]);

      object DupNode {
        def fromNode(n: Node) = {
          new DupNodeID(n.types, 
            n match {
              case _ : LNode => 0
              case _ : INode => 1
            },
            ptGraph.inEdges(n).map { 
              case e: IEdge => (0, e.label, e.v1)
              case e: OEdge => (1, e.label, e.v1)
            } ++
            ptGraph.outEdges(n).map {
              case e: IEdge => (2, e.label, e.v2)
              case e: OEdge => (3, e.label, e.v2)
            })
        }
      }

      val nodesGrouped: Map[DupNodeID, Set[Node]] = ptGraph.V.filter(n => n.isInstanceOf[INode] || n.isInstanceOf[LNode]).groupBy(DupNode.fromNode _)

      var newEnv = this

      var iEdgesToRemove = Set[IEdge]()
      var oEdgesToRemove = Set[OEdge]()
      var nodesToRemove  = Set[Node]()

      for ((id, nodes) <- nodesGrouped if nodes.size > 1) {
        val toGroup = nodes.toSeq
        var keep    = toGroup.head
        var remove  = nodes - keep

        keep match {
          case i: INode if i.sgt =>
            remove find { case n :INode  => !n.sgt } match {
              case Some(n) =>
                // We found a better node to keep
                keep   = n
                remove = remove - n + i
              case None =>
                // Special case, if we merge only singleton nodes, we replace them with a non-singleton one
                val newINode = i.copy(sgt = false)

                keep = newINode

                if (remove contains newINode) {
                  // we swapped nodes, so let's not remove it
                  remove -= newINode
                } else {
                  newEnv = newEnv.replaceNode(i, Set(newINode))
                  remove += i
                }
            }
            case _ =>
        }

        for (n <- remove) {
          nodesToRemove += n

          ptGraph.outEdges(n).foreach {
            case e: IEdge =>
              iEdgesToRemove += e
            case e: OEdge =>
              oEdgesToRemove += e
          }
          ptGraph.inEdges(n).foreach {
            case e: IEdge =>
              iEdgesToRemove += e
            case e: OEdge =>
              oEdgesToRemove += e
          }
        }


      }
      
      new PTEnv(new PointToGraph(newEnv.ptGraph.V -- nodesToRemove,
                                 newEnv.ptGraph.E -- iEdgesToRemove -- oEdgesToRemove),
                newEnv.locState.map{ case (k, ns) => (k, ns -- nodesToRemove) },
                newEnv.iEdges -- iEdgesToRemove,
                newEnv.oEdges -- oEdgesToRemove,
                newEnv.danglingCalls,
                newEnv.isBottom,
                newEnv.isEmpty);
    }

    if (locState.exists(_._2.isEmpty)) {
      println(locState)
      reporter.fatal("Empty mapping in locstate is not allowed in PTEnvs");
    }
  }

  object BottomPTEnv extends PTEnv(true, true)
  object EmptyPTEnv extends PTEnv(false, true)

  class PTEnvCopier() {
    val graphCopier: PTGraphCopier = new PTGraphCopier {
      override def copyRef(ref: CFG.Ref): CFG.Ref = PTEnvCopier.this.copyRef(ref)

      override def copyTypes(info: TypeInfo): TypeInfo = PTEnvCopier.this.copyTypes(info)

      override def copyNode(n: Node): Node = n match {
        case VNode(ref) =>
          n
        case LNode(fromNode, via, pPoint, sig) =>
          LNode(copyNode(fromNode), copyField(via), pPoint, copySigEntry(sig))
        case LVNode(ref, sig) =>
          LVNode(PTEnvCopier.this.copyRef(ref), copySigEntry(sig))
        case INode(pPoint, sgt, sym) =>
          INode(pPoint, sgt, PTEnvCopier.this.copySymbol(sym))
        case OBNode(sym) =>
          OBNode(PTEnvCopier.this.copySymbol(sym))
        case n: SimpleNode =>
          n
        case _ =>
          sys.error("Unnexpected node type at this point")
      }
    }

    def copyRef(ref: CFG.Ref): CFG.Ref = ref

    def copySymbol(sym: Symbol): Symbol = sym

    def copyTypes(info: TypeInfo): TypeInfo = info

    def copy(env: PTEnv): PTEnv = {
      PTEnv(
        graphCopier.copy(env.ptGraph),
        env.locState.foldLeft(Map[CFG.Ref, Set[Node]]().withDefaultValue(Set())){ case (map, (r, v)) => 
          val nk = copyRef(r)
          map + (nk -> (v.map(graphCopier.copyNode _) ++ map(nk)))
        },
        env.iEdges.map(graphCopier.copyIEdge _),
        env.oEdges.map(graphCopier.copyOEdge _),
        env.danglingCalls,
        env.isBottom,
        env.isEmpty
      )
    }
  }

  class PTEnvReplacer(typeMap: TypeMap, symbolMap: Map[Symbol, Symbol]) extends PTEnvCopier {
    override def copyRef(ref: CFG.Ref): CFG.Ref = ref match {
      case tr: CFG.ThisRef =>
        copyThisRef(tr)
      case CFG.TempRef(name, version, tpe)  =>
        CFG.TempRef(name, version, copyType(tpe))
      case CFG.ObjRef(sym, tpe) =>
        CFG.ObjRef(copySymbol(sym), copyType(tpe))
      case CFG.SymRef(sym, version, tpe) =>
        CFG.SymRef(copySymbol(sym), version, copyType(tpe))
    }

    def copyThisRef(ref: CFG.ThisRef): CFG.ThisRef = {
      CFG.ThisRef(copySymbol(ref.symbol), ref.version, copyType(ref.tpe))
    }

    override def copySymbol(sym: Symbol): Symbol = symbolMap.getOrElse(sym, sym)

    override def copyTypes(info: TypeInfo): TypeInfo = typeMap(info)

    def copyType(tpe: Type): Type= typeMap(tpe)

  }

}
