package insane
package alias

import utils.Graphs._
import utils._

import scala.reflect.generic.Flags

trait PointToGraphsDefs extends ModifyClauses {
  self: AnalysisComponent =>

  import global._

  object PointToGraphs {
    sealed abstract class Node(val name: String, val isSingleton: Boolean) extends VertexAbs {
      val types: ObjectSet
      val isResolved: Boolean
    }

    case class VNode(ref: CFG.Ref) extends Node(""+ref.toString+"", false) {
      val types = ObjectSet.empty
      val isResolved = true
    }

    trait GloballyReachableNode

    case class LVNode(ref: CFG.Ref, types: ObjectSet) extends Node("Loc("+ref+")", true) {
      val isResolved = false
    }
    case class INode(pPoint: UniqueID, sgt: Boolean, types: ObjectSet) extends Node("I(@"+pPoint+")", sgt) {
      val isResolved = true
    }

    // mutable fromNode is only used when unserializing
    case class LNode(var fromNode: Node, via: Field, pPoint: UniqueID, types: ObjectSet) extends Node("L"+pPoint, true) {
      val isResolved = false
    }

    case class OBNode(s: Symbol) extends Node("Obj("+s.name+")", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(s.tpe)
      val isResolved = true
    }

    def safeLNode(from: Node, via: Field, pPoint: UniqueID): Option[LNode] = {
      val types = from.types.exactTypes.flatMap { t =>
        val s = t.decl(via.name)

        if (s == NoSymbol) {
          None
        } else {
          Some(s.tpe)
        }
      } toSet

      if (types.isEmpty) {
        None
      } else {
        Some(LNode(from match { case LNode(lfrom, _, _, _) => lfrom case _ => from }, via, pPoint, ObjectSet(types, types)))
      }
    }

    case object GBNode extends Node("Ngb", false) with GloballyReachableNode {
      val types = ObjectSet.subtypesOf(definitions.ObjectClass)
      val isResolved = true
    }

    case object NNode extends Node("Null", true) with GloballyReachableNode {
      val types = ObjectSet.empty
      val isResolved = true
    }
    case object StringLitNode extends Node("StringLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.StringClass.tpe)
      val isResolved = true
    }
    case object LongLitNode extends Node("LongLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.LongClass.tpe)
      val isResolved = true
    }
    case object IntLitNode extends Node("IntLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.IntClass.tpe)
      val isResolved = true
    }
    case object FloatLitNode extends Node("FloatLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.FloatClass.tpe)
      val isResolved = true
    }
    case object ByteLitNode extends Node("ByteLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.ByteClass.tpe)
      val isResolved = true
    }
    case object CharLitNode extends Node("CharLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.CharClass.tpe)
      val isResolved = true
    }
    case object ShortLitNode extends Node("ShortLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.ShortClass.tpe)
      val isResolved = true
    }
    case object DoubleLitNode extends Node("DoubleLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.DoubleClass.tpe)
      val isResolved = true
    }
    case object BooleanLitNode extends Node("BooleanLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.BooleanClass.tpe)
      val isResolved = true
    }
    case object TrueLitNode extends Node("True", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.BooleanClass.tpe)
      val isResolved = true
    }
    case object FalseLitNode extends Node("False", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.BooleanClass.tpe)
      val isResolved = true
    }

    def typeToLitNode(t: Type): Node = 
      if (t == definitions.StringClass.tpe) {
        StringLitNode
      } else if (t == definitions.IntClass.tpe) {
        IntLitNode
      } else if (t == definitions.LongClass.tpe) {
        LongLitNode
      } else if (t == definitions.ShortClass.tpe) {
        ShortLitNode
      } else if (t == definitions.DoubleClass.tpe) {
        DoubleLitNode
      } else if (t == definitions.BooleanClass.tpe) {
        BooleanLitNode
      } else {
        NNode
      }

    sealed abstract class Edge(val v1: Node, val label: Field, val v2: Node) extends LabeledEdgeAbs[Field, Node] {
      override def toString() = v1+"-("+label+")->"+v2
    }

    object Edge {
      def unapply(e: Edge) = Some((e.v1, e.label, e.v2))
    }

    case class IEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class OEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class VEdge(_v1: VNode, _v2: Node) extends Edge(_v1, NoField, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    private def completeGraph(env: PTEnv) = {
        var newGraph = env.ptGraph

        // We complete the graph with local vars -> nodes association, for clarity
        for ((ref, nodes) <- env.locState; n <- nodes) {
          newGraph += VEdge(VNode(ref), n)
        }

        newGraph
    }

    class PTDotConverter(_graph: PointToGraph, _title: String, _prefix: String) extends DotConverter(_graph, _title, _prefix) {
      import utils.DotHelpers

      def this(env: PTEnv, _title: String, prefix: String = "") = 
        this(completeGraph(env), _title, prefix)

      def labelToString(f: Field): String = f.name

      override def edgeToString(res: StringBuffer, e: Edge) {
        e match {
          case VEdge(v1, v2) => // Variable edge, used to draw graphs only (var -> nodes)
            res append DotHelpers.arrow(vToS(e.v1), vToS(e.v2), List("arrowhead=vee", "color=blue4"))
          case IEdge(v1, l, v2) =>
            res append DotHelpers.labeledArrow(vToS(e.v1), labelToString(e.label), vToS(e.v2))
          case OEdge(v1, l, v2) =>
            res append DotHelpers.labeledDashedArrow(vToS(e.v1), labelToString(e.label), vToS(e.v2))
        }
      }

      override def vertexToString(res: StringBuffer, v: Node) {
        //var opts = if(returnNodes contains v) List("shape=doublecircle") else List("shape=circle")
        var opts = List("fontsize=10")

        v match {
          case VNode(ref) => // Variable node, used to draw graphs only (var -> nodes)
            res append DotHelpers.invisNode(vToS(v), v.name, "shape=rectangle" :: "fontcolor=blue4" :: opts)
          case LVNode(ref, _) =>
            res append DotHelpers.dashedNode(vToS(v), v.name+"\\n"+v.types, "shape=rectangle" :: "color=green" :: opts)
          case LNode(_, _, _, _) =>
            res append DotHelpers.dashedNode(vToS(v), v.name+"\\n"+v.types, "shape=rectangle" :: opts)
          case INode(pPoint, sgt, _) =>
            res append DotHelpers.node(vToS(v), v.name+"\\n"+v.types, (if(sgt) "shape=rectangle" else "shape=box3d") ::opts)
          case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | OBNode(_) | TrueLitNode | FalseLitNode =>
            res append DotHelpers.node(vToS(v), v.name, "shape=rectangle" :: opts)
        }
      }
    }

    class GraphCopier() {
      def copy(ptGraph: PointToGraph): PointToGraph = {
        new PointToGraph(ptGraph.V.map(copyNode _), ptGraph.E.map(copyEdge _))
      }

      def copyNode(n: Node): Node = n match {
        case VNode(ref) =>
          n
        case LNode(fromNode, via, pPoint, types) =>
          LNode(copyNode(fromNode), copyField(via), pPoint, copyTypes(types))
        case LVNode(ref, types) =>
          LVNode(ref, copyTypes(types))
        case INode(pPoint, sgt, types) =>
          INode(pPoint, sgt, copyTypes(types))
        case OBNode(sym) =>
          n
        case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | TrueLitNode | FalseLitNode =>
          n
        case _ =>
          sys.error("Unnexpected node type at this point")
      }

      def copyIEdge(ie: IEdge): IEdge =
          IEdge(copyNode(ie.v1), copyField(ie.label), copyNode(ie.v2))

      def copyOEdge(oe: OEdge): OEdge =
          OEdge(copyNode(oe.v1), copyField(oe.label), copyNode(oe.v2))

      def copyEdge(e: Edge): Edge = e match {
        case ie: IEdge =>
          copyIEdge(ie)
        case oe: OEdge =>
          copyOEdge(oe)
        case _ =>
          sys.error("Unnexpected edge type at this point")
      }

      def copyField(f: Field): Field = f

      def copyTypes(oset: ObjectSet): ObjectSet = oset
    }
  }
}
