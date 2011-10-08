package insane
package alias

import utils.Graphs._
import utils._

import scala.reflect.generic.Flags

trait PointToGraphsDefs extends ModifyClauses {
  self: AnalysisComponent =>

  import global._

  object PointToGraphs {
    sealed abstract class Node(val name: String, val isSingleton: Boolean) extends VertexAbs[Edge] {
      val types: ObjectSet
    }

    case class VNode(ref: CFG.Ref)                                     extends Node(""+ref.toString+"", false) {
      val types = ObjectSet.empty
    }

    trait GloballyReachableNode

    case class PNode(pId: Int, types: ObjectSet)                                         extends Node("P("+pId+")", true)
    case class LVNode(ref: CFG.Ref, types: ObjectSet)                                    extends Node("Loc("+ref+")", false)
    case class INode(pPoint: UniqueID, sgt: Boolean, types: ObjectSet)                   extends Node("I(@"+pPoint+")", sgt)
    case class LNode(var fromNode: Node, via: Field, pPoint: UniqueID, types: ObjectSet) extends Node("L"+pPoint, false)

    case class OBNode(s: Symbol) extends Node("Obj("+s.name+")", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(s.tpe)
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
    }

    case object NNode extends Node("Null", true) with GloballyReachableNode {
      val types = ObjectSet.empty
    }
    case object StringLitNode extends Node("StringLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.StringClass.tpe)
    }
    case object LongLitNode extends Node("LongLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.LongClass.tpe)
    }
    case object IntLitNode extends Node("IntLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.IntClass.tpe)
    }
    case object FloatLitNode extends Node("FloatLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.FloatClass.tpe)
    }
    case object ByteLitNode extends Node("ByteLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.ByteClass.tpe)
    }
    case object CharLitNode extends Node("CharLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.CharClass.tpe)
    }
    case object ShortLitNode extends Node("ShortLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.ShortClass.tpe)
    }
    case object DoubleLitNode extends Node("DoubleLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.DoubleClass.tpe)
    }
    case object BooleanLitNode extends Node("BooleanLit", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(definitions.BooleanClass.tpe)
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

    // Synthetic node, only to represent unanalyzed/dangling method calls
    case class DCallNode(obj: Set[Node], args: Seq[Set[Node]], symbol: Symbol) extends Node("call "+obj+"."+uniqueFunctionName(symbol)+args.mkString("(", ",", ")")+" ("+args+")", false) {
      lazy val types = methodReturnType(symbol)
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

    case class DCallObjEdge(_v1: Node, _v2: DCallNode) extends Edge(_v1, NoField, _v2)
    case class DCallArgEdge(_v1: Node, argIndex: Int, _v2: DCallNode) extends Edge(_v1, NoField, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    class PTDotConverter(_graph: PointToGraph, _title: String, returnNodes: Set[Node]) extends DotConverter(_graph, _title) {
      import utils.DotHelpers

      def labelToString(f: Field): String = f.name

      override def edgeToString(res: StringBuffer, e: Edge) {
        e match {
          case VEdge(v1, v2) => // Variable edge, used to draw graphs only (var -> nodes)
            res append DotHelpers.arrow(e.v1.dotName, e.v2.dotName, List("arrowhead=vee", "color=blue4"))
          case DCallObjEdge(v1, v2) => // Dangling call object edge, used to draw graphs only (node -> receiver)
            res append DotHelpers.labeledArrow(e.v1.dotName, "rec", e.v2.dotName, List("color=gold", "style=dotted"))
          case DCallArgEdge(v1, argIndex, v2) => // Dangling call Arg edge, used to draw graphs only (node -> args)
            res append DotHelpers.labeledArrow(e.v1.dotName, "arg "+argIndex, e.v2.dotName, List("color=gold", "style=dotted"))
          case IEdge(v1, l, v2) =>
            res append DotHelpers.labeledArrow(e.v1.dotName, labelToString(e.label), e.v2.dotName)
          case OEdge(v1, l, v2) =>
            res append DotHelpers.labeledDashedArrow(e.v1.dotName, labelToString(e.label), e.v2.dotName)
        }
      }

      override def vertexToString(res: StringBuffer, v: Node) {
        var opts = if(returnNodes contains v) List("shape=doublecircle") else List("shape=circle")

        opts = if (v.isSingleton) "color=blue3" :: opts else opts

        v match {
          case VNode(ref) => // Variable node, used to draw graphs only (var -> nodes)
            res append DotHelpers.invisNode(v.dotName, v.name, List("fontcolor=blue4"))
          case LVNode(ref, _) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, List("color=green"))
          case LNode(_, _, _, _) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case PNode(pPoint, _) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case INode(pPoint, _, _) =>
            res append DotHelpers.node(v.dotName, v.name, opts)
          case dCall: DCallNode =>
            res append DotHelpers.node(v.dotName, v.name, opts ::: "shape=rect" :: Nil)
          case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | OBNode(_) =>
            res append DotHelpers.node(v.dotName, v.name, opts)
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
        case PNode(pPoint, types) =>
          PNode(pPoint, copyTypes(types))
        case INode(pPoint, sgt, types) =>
          INode(pPoint, sgt, copyTypes(types))
        case OBNode(sym) =>
          n
        case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode =>
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
