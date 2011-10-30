package insane
package alias

import storage.Database

import utils._
import utils.Reporters._
import utils.Graphs.DotConverter
import CFG._

import scala.reflect.generic.Flags

trait PointToAnalysis extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  var cnt = 0

  def getPTEnvFromFunSym(sym: Symbol): Option[PTEnv] = funDecls.get(sym).map(_.pointToResult)

  var predefinedPriorityEnvs = Map[Symbol, Option[PTEnv]]()

  val ptProgressBar = reporter.getProgressBar(42);

  def getPredefPriorityEnv(sym: Symbol): Option[PTEnv] = predefinedPriorityEnvs.get(sym) match {
    case Some(optPTEnv) => optPTEnv
    case None =>
      if (Database.active) {
        val optEnv = Database.Env.lookupPriorityEnv(uniqueFunctionName(sym)).map(s => EnvUnSerializer(s).unserialize)
        predefinedPriorityEnvs += sym -> optEnv

        optEnv
      } else {
        None
      }
  }

  var predefinedEnvs = Map[Symbol, Option[PTEnv]]()

  def getPredefEnv(sym: Symbol): Option[PTEnv] = predefinedEnvs.get(sym) match {
    case Some(optPTEnv) => optPTEnv
    case None =>
      if (Database.active) {
        val optEnv = Database.Env.lookupEnv(uniqueFunctionName(sym)).map(s => EnvUnSerializer(s).unserialize)
        predefinedEnvs += sym -> optEnv
        optEnv
      } else {
        None
      }
  }

  def getPTEnv(sym: Symbol): Option[PTEnv] = {
    getPredefPriorityEnv(sym) orElse getPTEnvFromFunSym(sym) orElse getPredefEnv(sym)
  }

  def getAllTargetsUsing(edges: Traversable[Edge])(from: Set[Node], via: Field): Set[Node] = {
    edges.collect{ case Edge(v1, f, v2) if (from contains v1) && (f == via) => v2 }.toSet
  }

  class PTEnvCopier() {
    val graphCopier: GraphCopier = new GraphCopier

    def copyLocRef(ref: CFG.Ref): CFG.Ref = ref

    def copy(env: PTEnv): PTEnv = {
      PTEnv(
        graphCopier.copy(env.ptGraph),
        env.locState.foldLeft(Map[CFG.Ref, Set[Node]]().withDefaultValue(Set())){ case (map, (r, v)) => 
          val nk = copyLocRef(r)
          map + (nk -> (v.map(graphCopier.copyNode _) ++ map(nk)))
        },
        env.iEdges.map(graphCopier.copyIEdge _),
        env.oEdges.map(graphCopier.copyOEdge _),
        env.isPartial,
        env.isBottom
      )
    }
  }

  class PTEnvReplacer(typeMap: Map[Type, Type], symbolMap: Map[Symbol, Symbol]) extends PTEnvCopier {
    def newSymbol(s: Symbol) = symbolMap.getOrElse(s, s)
    def newType(t: Type)     = typeMap.getOrElse(t, t)

    override val graphCopier = new GraphCopier {
      override def copyNode(n: Node) = n match {
        case OBNode(s) =>
          OBNode(newSymbol(s))
        case _ =>
          super.copyNode(n)
      }

      override def copyTypes(oset: ObjectSet): ObjectSet = {
        ObjectSet(oset.subtypesOf.map(newType _), oset.exactTypes.map(newType _))
      }
    }

  }

  case class PTEnv(ptGraph: PointToGraph,
                 locState: Map[CFG.Ref, Set[Node]],
                 // typeInfo: Map[CFG.Ref, Option[ObjectSet]],
                 iEdges: Set[IEdge],
                 oEdges: Set[OEdge],
                 isPartial: Boolean,
                 isBottom: Boolean) extends dataflow.EnvAbs[PTEnv] {

    def this(isPartial: Boolean = false, isBottom: Boolean = false) =
      this(new PointToGraph(),
           Map().withDefaultValue(Set()),
           // Map().withDefaultValue(None),
           Set(),
           Set(),
           // Set(),
           isPartial,
           isBottom)

    def clean() = copy(locState = Map().withDefaultValue(Set()))

    val getAllTargets   = getAllTargetsUsing(ptGraph.E)_
    val getWriteTargets = getAllTargetsUsing(iEdges)_
    val getReadTargets  = getAllTargetsUsing(oEdges)_

    def setL(ref: CFG.Ref, nodes: Set[Node]) = {
      copy(locState = locState + (ref -> nodes), isBottom = false)
    }

    def getL(ref: CFG.Ref, readOnly: Boolean): (PTEnv, Set[Node]) = {
      if (locState contains ref) {
        (this, locState(ref))
      } else {
        if (readOnly) {
          reporter.error("Consistency problem: local field accessed without associated nodes in a comp-sub-graph while in read-only context");
          (this, locState(ref))
        } else {
          val n = LVNode(ref, ObjectSet.subtypesOf(ref.tpe))
          (addNode(n).setL(ref, Set(n)), Set(n))
        }
      }
    }

    def replaceNode(from: Node, toNodes: Set[Node]) = {
      assert(!(toNodes contains from), "Recursively replacing "+from+" with "+toNodes.mkString("{", ", ", "}")+"!")

      var newEnv = copy(ptGraph = ptGraph - from ++ toNodes, isBottom = false)

      // Update iEdges
      for (iEdge @ IEdge(v1, lab, v2) <- iEdges if v1 == from || v2 == from; to <- toNodes) {
        val newIEdge = IEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)
        
        newEnv = newEnv.copy(ptGraph = ptGraph - iEdge + newIEdge, iEdges = iEdges - iEdge + newIEdge)
      }

      // Update oEdges
      for (oEdge @ OEdge(v1, lab, v2) <- oEdges if v1 == from || v2 == from; to <- toNodes) {
        val newOEdge = OEdge(if (v1 == from) to else v1, lab, if (v2 == from) to else v2)
        
        newEnv = newEnv.copy(ptGraph = ptGraph - oEdge + newOEdge, oEdges = oEdges - oEdge + newOEdge)
      }

      // Update locState
      newEnv = newEnv.copy(locState = locState.map{ case (ref, nodes) => ref -> (if (nodes contains from) nodes - from ++ toNodes else nodes) }.withDefaultValue(Set()))

      newEnv
    }

    def addNode(node: Node) =
      copy(ptGraph = ptGraph + node, isBottom = false)

    lazy val loadNodes: Set[LNode] = {
      ptGraph.V.collect { case l: LNode => l }
    }

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
          safeLNode(node, field, uniqueID) match {
            case Some(lNode) =>
              res = res.addNode(lNode).addOEdge(node, field, lNode)
              pointResults += lNode
            case None =>
              reporter.error("Unable to create LNode from "+node+" via "+field)
          }
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
            safeLNode(node, field, new UniqueID(0)) match {
              case Some(lNode) =>
                newEnv = newEnv.addNode(lNode).addOEdge(node, field, lNode).addIEdge(node, field, lNode)
              case None =>
                reporter.error("Unable to create LNode from "+node+" via "+field)
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

    def addGlobalNode() = {
      copy(ptGraph = ptGraph + GBNode, isBottom = false)
    }

    def stripTypeInconsistencies = {
      // TODO
      this
    }

    def modifiesClause: ModifyClause = {
      import scala.collection.mutable.Stack

      /**
       * Check if there is any reachable IEdge from
       * 1) Params
       * 2) Global Objects
       **/

      var seen    = Set[Node]()
      var effects = Set[ModifyClauseEffect]()

      for (n <- ptGraph.V) n match {
        // At this point, remaining LVNodes are parameters
        case _: LVNode | _: GloballyReachableNode =>
          visitRoot(n)
        case _ =>
      }

      def visitRoot(n: Node) {
        def visit(n: Node, root: Node, path: List[Field]) {

          seen += n

          for (e @ Edge(v1, via, v2) <- ptGraph.outEdges(n)) {
            val newPath = via :: path

            e match {
              case _: IEdge =>
                effects += ModifyClauseEffect(newPath.reverse, root)
              case _ =>
            }

            if (!seen(v2)) {
              visit(v2, root, newPath)
            }
          }
        }

        visit(n, n, Nil)
      }

      ModifyClause(effects)
    }

    def duplicate = this

    def getNodes(sv: CFG.SimpleValue, readonly: Boolean = false): (PTEnv, Set[Node]) = sv match {
      case r2: CFG.Ref       => getL(r2, readonly)
      case n : CFG.Null      => (this, Set(NNode))
      case u : CFG.Unit      => (this, Set())
      case _: CFG.StringLit  => (this, Set(StringLitNode))
      case _: CFG.BooleanLit => (this, Set(BooleanLitNode))
      case _: CFG.LongLit    => (this, Set(LongLitNode))
      case _: CFG.IntLit     => (this, Set(IntLitNode))
      case _: CFG.CharLit    => (this, Set(CharLitNode))
      case _: CFG.ByteLit    => (this, Set(ByteLitNode))
      case _: CFG.FloatLit   => (this, Set(FloatLitNode))
      case _: CFG.DoubleLit  => (this, Set(DoubleLitNode))
      case _: CFG.ShortLit   => (this, Set(ShortLitNode))
    }

  }

  object BottomPTEnv extends PTEnv(false, true)

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    object PointToLattice extends dataflow.LatticeAbs[PTEnv] {
      val bottom = BottomPTEnv

      def join(envs: PTEnv*): PTEnv = {
        if(envs.size == 1) {
          return envs.head
        }

        /**
         * When merging environment, we need to take special care in case one
         * write edge is not present in the other envs, in that case, it
         * consists of a weak update in the resulting env.
         */

        var newIEdges = envs.flatMap(_.iEdges).toSet
        var newOEdges = envs.flatMap(_.oEdges).toSet
        var newNodes  = envs.flatMap(_.ptGraph.V).toSet

        // 1) We find all nodes that are shared between all envs
        val commonNodes = envs.map(_.ptGraph.V).reduceRight(_ & _)

        // 2) We find all the pair (v1, f) that are not in every env's iEdges
        val allPairs = newIEdges.map(ed => (ed.v1, ed.label)).toSet

        val commonPairs = envs.map(_.iEdges.map(ed => (ed.v1, ed.label)).toSet).reduceRight(_ & _)

        for ((v1, field) <- allPairs -- commonPairs if commonNodes contains v1) {
          // TODO: Is there already a load node for this field?
          safeLNode(v1, field, new UniqueID(0)) match {
            case Some(lNode) =>
              newNodes  += lNode
              newIEdges += IEdge(v1, field, lNode)
              newOEdges += OEdge(v1, field, lNode)
            case None =>
              reporter.error("Unable to create LNode from "+v1+" via "+field)
          }
        }

        val newGraph = new PointToGraph().copy(edges = Set[Edge]() ++ newOEdges ++ newIEdges, vertices = newNodes)

        val env = new PTEnv(
          newGraph,
          envs.flatMap(_.locState.keySet).toSet.map((k: CFG.Ref) => k -> (envs.map(e => e.locState(k)).reduceRight(_ ++ _))).toMap.withDefaultValue(Set()),
          // envs.flatMap(_.typeInfo.keySet).toSet.map((k: CFG.Ref) => k -> Some(envs.map(e => e.typeInfo(k)).collect{ case Some(s) => s }.reduceRight(_ ++ _))).toMap.withDefaultValue(None),
          newIEdges,
          newOEdges,
          envs.exists(_.isPartial),
          false)

        env
      }

    }

    class PointToTF(fun: AbsFunction) extends dataflow.TransferFunctionAbs[PTEnv, CFG.Statement] {

      var analysis: dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG] = null

      def apply(edge: CFGEdge[CFG.Statement], oldEnv: PTEnv): PTEnv = {
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

          def +++(ns: Seq[(Node, Set[Node])]) = {
            copy(map = map ++ (ns.map(nn => (nn._1 -> (map(nn._1) ++ nn._2)))))
          }
        }

        def mergeGraphs(outerG: PTEnv, innerG: PTEnv, uniqueID: UniqueID, pos: Position, allowStrongUpdates: Boolean): PTEnv = {

          if (outerG.isBottom) {
            innerG
          } else {

            cnt += 1
            if (settings.dumpPTGraph(safeFullName(fun.symbol))) {
              reporter.msg("    Merging graphs ("+cnt+")...")
              new PTDotConverter(outerG, "Before - "+cnt).writeFile("before-"+cnt+".dot")
              new PTDotConverter(innerG, "Inner - "+cnt).writeFile("inner-"+cnt+".dot")
            }

            // Build map
            var newOuterG = outerG;

            // 1) We build basic nodemap

            var nodeMap: NodeMap = NodeMap()
            for (n <- GBNode :: NNode :: NNode :: BooleanLitNode :: LongLitNode :: DoubleLitNode :: StringLitNode :: IntLitNode :: ByteLitNode :: CharLitNode :: FloatLitNode :: ShortLitNode :: Nil if innerG.ptGraph.V.contains(n)) {
              nodeMap += n -> n
            }

            // 2) We add all singleton object nodes to themselves
            for (n <- innerG.ptGraph.V.filter(_.isInstanceOf[OBNode])) {
              nodeMap += n -> n
            }

            // 3) We map local variables-nodes to the corresponding outer ones
            for (n <- innerG.ptGraph.V.filter(_.isInstanceOf[LVNode])) {
              val ref = n.asInstanceOf[LVNode].ref

              val (newEnv, nodes) = newOuterG.getL(ref, false);
              newOuterG = newEnv

              nodeMap ++= (n -> nodes)
            }

            // 4) Inline Inside nodes with refinement of the allocation site
            // TODO: See if this shouldn't be at the inlining stage
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
              } else {
                newOuterG = newOuterG.addNode(iNodeUnique)
                iNodeUnique
              }
            }

            // Map all inside nodes to themselves
            nodeMap +++= innerG.ptGraph.vertices.toSeq.collect{ case n: INode => (n: Node,Set[Node](inlineINode(n))) }

            // 5) Resolve load nodes
            def resolveLoadNode(lNode: LNode): Set[Node] = {
              val LNode(from, field, pPoint, types) = lNode

              val fromNodes = from match {
                case l : LNode =>
                  resolveLoadNode(l)
                case _ =>
                  nodeMap(from)
              }

              var pointedResults = Set[Node]()

              for (node <- fromNodes) {
                val writeTargets = newOuterG.getWriteTargets(Set(node), field)

                val pointed = if (writeTargets.isEmpty) {
                  newOuterG.getReadTargets(Set(node), field)
                } else {
                  writeTargets
                }

                if (pointed.isEmpty) {
                  val newId = pPoint safeAdd uniqueID

                  safeLNode(node, field, newId) match {
                    case Some(lNode) =>
                      newOuterG = newOuterG.addNode(lNode).addOEdge(node, field, lNode)
                      pointedResults += lNode
                    case None =>
                      // Ignore incompatibility
                  }
                } else {
                  pointedResults ++= pointed
                }
              }

              pointedResults
            }

            for (lNode <- innerG.loadNodes) {
              nodeMap ++= lNode -> resolveLoadNode(lNode)
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

            if (settings.dumpPTGraph(safeFullName(fun.symbol))) {
              reporter.msg("    NodeMap: "+nodeMap)
              new PTDotConverter(newOuterG, "Inter - "+cnt).writeFile("inter-"+cnt+".dot")
            }

            newOuterG = applyInnerEdgesFixPoint(innerG, newOuterG, nodeMap)

            if (settings.dumpPTGraph(safeFullName(fun.symbol))) {
              new PTDotConverter(newOuterG, "new - "+cnt).writeFile("new-"+cnt+".dot")
            }
            cnt += 1

            newOuterG
          }
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

            val (newEnv, nodes)   = env.getNodes(aam.obj)

            val oset = aam.obj match {
              case CFG.SuperRef(sym, _) =>
                ObjectSet.singleton(sym.superClass.tpe)
              case _ =>
                (ObjectSet.empty /: nodes) (_ ++ _.types)
            }

            val name = uniqueFunctionName(fun.symbol);

            val targets = getMatchingMethods(aam.meth, oset.resolveTypes, aam.pos, aam.isDynamic)

            checkIfInlinable(aam.meth, oset, targets) match {
              case None =>
                // 1) Gather CFGs of targets
                val existingTargets = targets flatMap { sym =>
                  funDecls.get(sym) match {
                    case None =>
                      reporter.warn("Could not gather pt-CFG of "+sym.name+" ("+uniqueFunctionName(sym)+"), ignoring.")
                      None
                    case e =>
                      e
                  }
                }

                var cfg = analysis.cfg

                settings.ifDebug {
                  reporter.msg("  Ready to inline for : "+aam+", "+existingTargets.size+" targets available")
                }

                val nodeA = edge.v1
                val nodeB = edge.v2

                // 2) Remove current edge
                cfg -= edge

                /**
                 * We replace
                 *   nodeA -- r = call(arg1,...argN) -- nodeB
                 * into:
                 *   nodeA -- arg1=Farg1 -- ... argN--FargN -- rename(CFG of Call) -- r = retval -- nodeB
                 */

                for (sym <- targets) {
                  funDecls.get(sym) match {
                    case None =>
                      reporter.warn("Could not gather pt-CFG of "+sym.name+" ("+uniqueFunctionName(sym)+"), ignoring.")
                      cfg += (nodeA, CFG.Skip, nodeB)
                    case Some(targetFun) =>
                      val targetCFG = targetFun.ptcfg

                      var map = Map[CFGTrees.Ref, CFGTrees.Ref]()

                      var connectingEdges = Set[CFG.Statement]()

                      // 1) Build renaming map:
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

                      // 2) Rename targetCFG
                      val renamedCFG = new FunctionCFGRefRenamer(map).copy(targetCFG)

                      // 3) Connect renamedCFG to the current CFG
                      if (connectingEdges.isEmpty) {
                        // If no arg was explicitely mapped via assigns, we still need to connect to the CFG
                        cfg += (nodeA, CFG.Skip, renamedCFG.entry)
                      } else {
                        for(stmt <- connectingEdges) {
                          cfg += (nodeA, stmt, renamedCFG.entry)
                        }
                      }

                      // 4) Adding CFG Edges
                      for (tEdge <- renamedCFG.graph.E) {
                        cfg += tEdge
                      }

                      // 5) Retval has been mapped via renaming, simply connect it
                      cfg += (renamedCFG.exit, CFG.Skip, nodeB)
                  }
                }

                settings.ifDebug {
                  reporter.info("  Restarting...")
                }
                cnt += 1

                cfg = cfg.removeSkips.removeIsolatedVertices
                if (settings.dumpPTGraph(safeFullName(fun.symbol))) {
                  new CFGDotConverter(cfg, "work").writeFile(uniqueFunctionName(fun.symbol)+"-work.dot")
                }

                fun.setPTCFG(cfg)
                analysis.restartWithCFG(cfg)

              case Some((reason, isError)) =>
                aam.obj match {
                  case CFG.SuperRef(sym, _) =>
                    reporter.error(List(
                      "Cannot inline/delay call to super."+sym.name+" ("+uniqueFunctionName(sym)+"), ignoring call.",
                      "Reason: "+reason), aam.pos)

                    // From there on, the effects are partial graphs
                    env = new PTEnv(true, false)
                  case _ =>
                    if (isError) {
                      reporter.error(List("Cannot inline/delay call "+aam+", ignoring call.",
                        "Reason: "+reason), aam.pos)
                    } else {
                      reporter.warn(List("Delaying call to "+aam+"",
                        "Reason: "+reason), aam.pos)
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
            val (newEnv, nodes) = env.getNodes(ac.rhs)
            env = newEnv.setL(ac.r, nodes)

          case _ =>
        }

        env
      }

    }

    def checkIfInlinable(symbol: Symbol, oset: ObjectSet, targets: Set[Symbol]): Option[(String, Boolean)] = {
      if (!oset.isExhaustive && !settings.wholeCodeAnalysis) {
        Some("unbouded number of targets", true)
      } else if (targets.isEmpty) {
        Some("no target could be found", true)
      } else {
        if (targets.size > 3) {
          Some("too many targets ("+targets.size+")", false)
        } else {
          val unanalyzable = targets.filter(t => getPTEnv(t).isEmpty)

          if (!unanalyzable.isEmpty) {
            Some("some targets are unanalyzable: "+unanalyzable.map(uniqueFunctionName(_)).mkString(", "), true)
          } else {
            None
          }
        }
      }
    }

    def analyze(fun: AbsFunction) = {
      var cfg = fun.cfg
      var baseEnv    = new PTEnv()

      val name = uniqueFunctionName(fun.symbol)

      new CFGDotConverter(cfg, "init-CFG For "+name).writeFile(name+"-w1.dot")

      settings.ifVerbose {
        reporter.msg("Analyzing "+fun.uniqueName+"...")
      }

      if (settings.debugFunction(uniqueFunctionName(fun.symbol))) {
        settings.extensiveDebug = true
      }

      // 1) We add 'this'/'super'
      val thisNode = cfg.ptArgs(0)
      baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

      for (sr <- cfg.superRefs) {
        baseEnv = baseEnv.setL(sr, Set(thisNode))
      }

      // 2) We add arguments
      for ((a, i) <- cfg.args.zipWithIndex) {
        val aNode = cfg.ptArgs(i+1)
        baseEnv = baseEnv.addNode(aNode).setL(a, Set(aNode))
      }

      // 3) If we are in the constructor, we assign all fields defined by this class to their default value
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

      cfg += CFGEdge(cfg.entry, new CFGTrees.Effect(baseEnv, "Base: "+uniqueFunctionName(fun.symbol)) setTree fun.body, bstr)

      // 6) We run a fix-point on the CFG
      val ttf = new PointToTF(fun)
      val aa = new dataflow.Analysis[PTEnv, CFG.Statement, FunctionCFG](PointToLattice, PointToLattice.bottom, settings, cfg)

      ttf.analysis = aa

      aa.computeFixpoint(ttf)

      // Analysis CFG might have expanded
      cfg = aa.cfg

      val res = aa.getResult

      fun.pointToInfos  = res

      val e = res(cfg.exit)

      // 7) We reduce the result
      val ptCFG = if (e.isPartial) {
        // TODO: partial reduce
        cfg
      } else {
        var reducedCFG = new FunctionCFG(fun.symbol, cfg.args, cfg.retval)
        reducedCFG += (reducedCFG.entry, new CFGTrees.Effect(e, "Sum: "+uniqueFunctionName(fun.symbol)) setTree fun.body, reducedCFG.exit)
        reducedCFG
      }

      fun.setPTCFG(ptCFG)

      if (settings.dumpPTGraph(safeFullName(fun.symbol))) {
        val name = uniqueFunctionName(fun.symbol)
        val dest = name+"-ptcfg.dot"

        reporter.msg("  Dumping pt-CFG to "+dest+"...")
        new CFGDotConverter(ptCFG, "pt-CFG For "+name).writeFile(dest)
      }

      settings.ifVerbose {
        reporter.msg("  Done analyzing "+fun.uniqueName+"...")
      }

      if (settings.fillGraphs && settings.fillGraphsIteratively) {
        fillPartial(fun)
      }

      settings.extensiveDebug = false

      res
    }

    def analyzeSCC(scc: Set[Symbol]) {
      // The analysis is only run on symbols that are actually AbsFunctions, not all method symbols

      var workList = scc

      // 1) First, we remove from the worklist functions that we cannot analyze
      for (sym <- scc if !(funDecls contains sym)) {
        if (getPTEnv(sym).isEmpty) {
          reporter.warn("Ignoring the analysis of unknown methods: "+uniqueFunctionName(sym), sym.pos)
        }
        workList -= sym
      }

      // 2) Then, we analyze every methods until we reach a fixpoint
      while(!workList.isEmpty) {
        val sym = workList.head
        workList = workList.tail

        ptProgressBar.draw()

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
                      reporter.error("Could not understand annotation: "+annot, symbol.pos)
                      None
                  }
                } catch {
                  case e =>
                    reporter.error("Unable to find class symbol from name "+name+": "+e.getMessage)
                    None
                }
              case _ =>
                reporter.error("Could not understand annotation: "+annot, symbol.pos)
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
                reporter.error("Could not understand annotation: "+annot, symbol.pos)
                None
            }
          case None =>
            None
        }
    }

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

            /*
            val iNode = INode(new UniqueID(0), true, methodReturnType(fun.symbol))
            env = env.addNode(iNode).copy(rNodes = Set(iNode))
            fun.pointToResult = env
            */
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

    def fillPartial(fun: AbsFunction) {
      if (Database.active) {
        val (name, e, isSynth) = getResultEnv(fun)

        val toInsert = List((name, new EnvSerializer(e).serialize(), isSynth))

        Database.Env.insertAll(toInsert)
      } else {
        reporter.error("Cannot insert into database: No database configuration")
      }
    }

    def run() {
      // 1) Define symbols that have no chance of ever being implemented/defined
      predefinedEnvs += definitions.getMember(definitions.ObjectClass, nme.CONSTRUCTOR) -> Some(BottomPTEnv)

      // 1.5) Check symbols that will not be able to be analyzed:
      settings.ifDebug {
        for (scc <- callGraphSCCs.map(scc => scc.vertices.map(v => v.symbol)); sym <- scc) {
          if (!(funDecls contains sym)) {
            reporter.warn("No code for method: "+uniqueFunctionName(sym))
          }
        }
      }

      // 2) Analyze each SCC in sequence, in the reverse order of their topological order
      //    We first analyze {M,..}, and then methods that calls {M,...}
      val workList = callGraphSCCs.reverse.map(scc => scc.vertices.map(v => v.symbol))
      val totJob   = workList.map(_.size).sum

      ptProgressBar.setMax(totJob)
      ptProgressBar.draw()

      for (scc <- workList) {
        analyzeSCC(scc)
        ptProgressBar ticks scc.size
        ptProgressBar.draw()
      }

      ptProgressBar.end();

      // 3) Fill graphs in the DB, if asked to
      if (settings.fillGraphs && !settings.fillGraphsIteratively) {
        fillDatabase()
      }

      // 4) Display/dump results, if asked to
      if (!settings.dumpptgraphs.isEmpty) {
        for ((s, fun) <- funDecls if settings.dumpPTGraph(safeFullName(s))) {
          /* TODO: Dump PT-CFG Graphs

          val (name, e, _) = getResultEnv(fun)

          val dest = name+"-pt.dot"
          reporter.msg("Dumping Point-To Graph to "+dest+"...")
          new PTDotConverter(e, "Point-to: "+name).writeFile(dest)
          */
        }
      }

      settings.drawpt match {
        case Some(name) =>
          if (Database.active) {
            Database.Env.lookupEnv(name).map(s => EnvUnSerializer(s).unserialize) match {
              case Some(e) =>
                val dest = name+"-pt.dot"

                reporter.msg("Dumping Point-To Graph to "+dest+"...")
                new PTDotConverter(e, "Point-to: "+name).writeFile(dest)
              case None =>
                reporter.error("Could not find "+name+" in database!")
            }
          } else {
            reporter.error("Could not find "+name+" in database: No database connection!")
          }
        case None =>
      }

      if (!settings.displaypure.isEmpty) {
        reporter.title(" Purity Results:")
        for ((s, fun) <- funDecls if settings.displayPure(safeFullName(s))) {

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
        }
      }
    }
  }
}

