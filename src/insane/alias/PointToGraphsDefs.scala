package insane
package alias

import utils.Graphs._
import utils._

trait PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._

  object PointToGraphs {
    sealed abstract class Field {
      val symbol: Symbol
    }

    case class SymField(symbol: Symbol) extends Field {
      override def toString() = symbol.name.toString.trim
    }

    case object ArrayFields extends Field {
      override def toString() = "[*]"
      val symbol = NoSymbol
    }

    sealed abstract class Node(val name: String, val isSingleton: Boolean) extends VertexAbs[Edge] {
      val types: ObjectSet

      def deflate: DeflatedNode = sys.error("TODO")
    }

    sealed abstract class DeflatedNode(
        val id: Int,
        val nodeType: String,
        val name: String,
        val isSingleton: Boolean,
        val types: DeflatedObjectSet,
        val pId: Int,
        val pPoint: UniqueID,
        val via: String,
        val fromNode: String
      ) {

      def inflate: Node
    }

    sealed abstract class DeflatedEdge(nodeFrom: Int, nodeTo: Int, label: String, tpe: String) {
      def inflate(nodeMap: Map[Int, Node]): Edge = {
        tpe match {
          case "I" => 
            IEdge(nodeMap(nodeFrom), SymField(NoSymbol), nodeMap(nodeTo))
          case "O" => 
            OEdge(nodeMap(nodeFrom), SymField(NoSymbol), nodeMap(nodeTo))
        }
      }
    }

    case class VNode(ref: CFG.Ref)                                     extends Node(""+ref.toString+"", false) {
      lazy val types = ObjectSet.empty
    }

    case class PNode(pId: Int, types: ObjectSet)                       extends Node("P("+pId+")", true)
    case class INode(pPoint: UniqueID, sgt: Boolean, types: ObjectSet) extends Node("I(@"+pPoint+")", sgt)
    case class LNode(fromNode: Node, via: Field, pPoint: UniqueID)     extends Node("L"+pPoint, false) {
      lazy val types = ObjectSet.subtypesOf(via.symbol)
    }

    def safeLNode(from: Node, via: Field, pPoint: UniqueID) = LNode(from match { case LNode(lfrom, _, _) => lfrom case _ => from }, via, pPoint)

    case object GBNode extends Node("Ngb", false) {
      val types = AllObjects
    }
    case object NNode extends Node("Null", true) {
      val types = ObjectSet.empty
    }
    case object StringLitNode extends Node("StringLit", true) {
      val types = ObjectSet.singleton(definitions.StringClass.tpe)
    }
    case object LongLitNode extends Node("LongLit", true) {
      val types = ObjectSet.singleton(definitions.LongClass.tpe)
    }
    case object IntLitNode extends Node("IntLit", true) {
      val types = ObjectSet.singleton(definitions.IntClass.tpe)
    }
    case object FloatLitNode extends Node("FloatLit", true) {
      val types = ObjectSet.singleton(definitions.FloatClass.tpe)
    }
    case object ByteLitNode extends Node("ByteLit", true) {
      val types = ObjectSet.singleton(definitions.ByteClass.tpe)
    }
    case object CharLitNode extends Node("CharLit", true) {
      val types = ObjectSet.singleton(definitions.CharClass.tpe)
    }
    case object DoubleLitNode extends Node("DoubleLit", true) {
      val types = ObjectSet.singleton(definitions.DoubleClass.tpe)
    }
    case object BooleanLitNode extends Node("BooleanLit", true) {
      val types = ObjectSet.singleton(definitions.BooleanClass.tpe)
    }

    def typeToLitNode(t: Type): Node = 
      if (t == definitions.StringClass.tpe) {
        StringLitNode
      } else if (t == definitions.IntClass.tpe) {
        IntLitNode
      } else if (t == definitions.LongClass.tpe) {
        LongLitNode
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

      def deflate(nodeMap: Map[Node, DeflatedNode]): DeflatedEdge = sys.error("TODO")
    }

    object Edge {
      def unapply(e: Edge) = Some((e.v1, e.label, e.v2))
    }

    case class IEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class OEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class VEdge(_v1: VNode, _v2: Node) extends Edge(_v1, SymField(NoSymbol), _v2)

    case class DCallObjEdge(_v1: Node, _v2: DCallNode) extends Edge(_v1, SymField(NoSymbol), _v2)
    case class DCallArgEdge(_v1: Node, argIndex: Int, _v2: DCallNode) extends Edge(_v1, SymField(NoSymbol), _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    class PTDotConverter(_graph: PointToGraph, _title: String, returnNodes: Set[Node]) extends DotConverter(_graph, _title) {
      import utils.DotHelpers

      def labelToString(f: Field): String = f match {
        case SymField(sym) =>
          sym.name.toString
        case ArrayFields =>
          "[*]"
      }

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
          case LNode(_, _, _) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case PNode(pPoint, _) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case INode(pPoint, _, _) =>
            res append DotHelpers.node(v.dotName, v.name, opts)
          case dCall: DCallNode =>
            res append DotHelpers.node(v.dotName, v.name, opts ::: "shape=rect" :: Nil)
          case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode =>
            res append DotHelpers.node(v.dotName, v.name, opts)
        }
      }
    }
  }
}
