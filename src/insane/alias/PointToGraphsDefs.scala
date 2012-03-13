package insane
package alias

import utils.Graphs._
import utils._

import scala.tools.nsc.symtab.Flags

trait PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._

  sealed case class Field(var fullName: String, strName: String) {
    val name: Name = newTermName(strName)
  }
  object NoField extends Field(NoSymbol.fullName, NoSymbol.name.toString)

  object Field {
    def apply(sym: Symbol) = new Field(sym.fullName, sym.name.toString)
  }


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

    case class LVNode(ref: CFG.Ref, types: ObjectSet) extends Node("Loc("+ref+")["+types+"]", true) {
      val isResolved = false
    }
    case class INode(pPoint: UniqueID, sgt: Boolean, types: ObjectSet) extends Node("I(@"+pPoint+")", sgt) {
      val isResolved = true
    }

    // mutable fromNode is only used when unserializing
    case class LNode(var fromNode: Node, via: Field, pPoint: UniqueID, types: ObjectSet) extends Node("L"+pPoint+"["+types+"]", true) {
      val isResolved = false
    }

    case class OBNode(s: Symbol) extends Node("Obj("+s.name+")", true) with GloballyReachableNode {
      val types = ObjectSet.singleton(s.tpe)
      val isResolved = true
    }

    def findSimilarLNodes(lNode: LNode, others: Set[Node]): Set[LNode] = {
      Set(lNode) ++ others.collect {
        case l: LNode if (l != lNode) &&
                         (l.fromNode, l.via, l.pPoint) == (lNode.fromNode, lNode.via, lNode.pPoint) &&
                         (l.types isMorePreciseThan lNode.types) =>
          l
      }
    }

    def safeLNode(from: Node, via: Field, pPoint: UniqueID): Option[LNode] = {
      val types = from.types.exactTypes.flatMap { t =>
        val s = t.decl(via.name)

        if (s == NoSymbol) {
          //reporter.debug(t+".decl("+via.name+") == NoSymbol") 
          None
        } else {
          Some(t.memberType(s))
        }
      } toSet

      if (types.isEmpty) {
        None
      } else {
        Some(safeTypedLNode(ObjectSet(types, types), from, via, pPoint))
      }
    }


    def safeTypedLNode(types: ObjectSet, from: Node, via: Field, pPoint: UniqueID): LNode = {
      LNode(from match { case LNode(lfrom, _, _, _) => lfrom case _ => from }, via, pPoint, types)
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
      } else if (t == definitions.LongClass.tpe) {
        LongLitNode
      } else if (t == definitions.IntClass.tpe) {
        IntLitNode
      } else if (t == definitions.FloatClass.tpe) {
        FloatLitNode
      } else if (t == definitions.ByteClass.tpe) {
        ByteLitNode
      } else if (t == definitions.CharClass.tpe) {
        CharLitNode
      } else if (t == definitions.ShortClass.tpe) {
        ShortLitNode
      } else if (t == definitions.DoubleClass.tpe) {
        DoubleLitNode
      } else if (t == definitions.BooleanClass.tpe) {
        BooleanLitNode
      } else {
        NNode
      }

    def buildPureEffect(sym: Symbol): FunctionCFG = {
      val (args, argsTypes, retval) = sym.tpe match {
        case MethodType(argssym, tpe) =>
          (argssym.map(s => new CFGTrees.SymRef(s, NoUniqueID, s.tpe)), argssym.map(s => ObjectSet.subtypesOf(s.tpe)), new CFGTrees.TempRef("retval", NoUniqueID, tpe))

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

      // 3) return value
      val retOset = ObjectSet.subtypesOf(retval.tpe)
      val retNode = if (isGroundOSET(retOset)) {
        typeToLitNode(retval.tpe)
      } else {
        INode(NoUniqueID, false, retOset)
      }

      baseEnv = baseEnv.addNode(retNode).setL(retval, Set(retNode))

      cfg += (cfg.entry, new CFGTrees.Effect(baseEnv, "Pure Effect of "+uniqueFunctionName(sym)) setTree EmptyTree, cfg.exit)

      cfg
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

    case class DNode(aam: CFG.AssignApplyMeth) extends Node(""+aam.toString+"", false) {
      val types = ObjectSet.empty
      val isResolved = true
    }
    case class DEdge(_v1: Node, _label: String, _v2: Node) extends Edge(_v1, NoField, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    private def completeGraph(env: PTEnv, noopCalls: Map[CFG.AssignApplyMeth, Seq[Set[Node]]]) = {
      var newGraph = env.ptGraph

      // We complete the graph with local vars -> nodes association, for clarity
      for ((ref, nodes) <- env.locState; n <- nodes) {
        newGraph += VEdge(VNode(ref), n)
      }

      // Let's add Dangling calls info
      for ((aam, args) <- noopCalls) {
        val dn = DNode(aam)
        newGraph += dn

        val retNodes = args.head
        for (n <- retNodes) {
          newGraph += DEdge(dn, "ret", n)
        }
        val argsNodes = args.tail

        for ((nodes, arg) <- argsNodes zip aam.args; n <- nodes) {
          newGraph += n
          newGraph += DEdge(n, arg.toString, dn)
        }
      }

      newGraph
    }

    def dumpPTE(env: PTEnv, dest: String) {
      reporter.debug("Dumping Effect to "+dest+"...")
      new PTDotConverter(env, "Effect", "", env.noopCalls).writeFile(dest)
    }

    class PTDotConverter(_graph: PointToGraph, _title: String, _prefix: String,
                         noopCalls: Map[CFG.AssignApplyMeth, Seq[Set[Node]]]) extends DotConverter(_graph, _title, _prefix) {
      import utils.DotHelpers

      def this(env: PTEnv, _title: String, prefix: String = "", noopCalls: Map[CFG.AssignApplyMeth, Seq[Set[Node]]] = Map()) = 
        this(completeGraph(env, noopCalls), _title, prefix, noopCalls)

      def labelToString(f: Field): String = f.strName

      override def edgeToString(res: StringBuffer, e: Edge) {
        e match {
          case VEdge(v1, v2) => // Variable edge, used to draw graphs only (var -> nodes)
            res append DotHelpers.arrow(vToS(e.v1), vToS(e.v2), List("arrowhead=vee", "color=blue4"))
          case IEdge(v1, l, v2) =>
            res append DotHelpers.labeledArrow(vToS(e.v1), labelToString(e.label), vToS(e.v2))
          case OEdge(v1, l, v2) =>
            res append DotHelpers.labeledDashedArrow(vToS(e.v1), labelToString(e.label), vToS(e.v2))
          case DEdge(v1, l, v2) =>
            res append DotHelpers.labeledArrow(vToS(e.v1), labelToString(e.label), vToS(e.v2), List("arrowhead=vee", "color=red4"))
        }
      }

      override def vertexToString(res: StringBuffer, v: Node) {
        //var opts = if(returnNodes contains v) List("shape=doublecircle") else List("shape=circle")
        var opts = List("fontsize=10")

        v match {
          case VNode(ref) => // Variable node, used to draw graphs only (var -> nodes)
            res append DotHelpers.invisNode(vToS(v), v.name, "fontcolor=blue4" :: opts)
          case DNode(aam) =>
            res append DotHelpers.node(vToS(v), aam.toString, "shape=box3d" :: "color=red4" :: opts)
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

    class PTGraphCopier extends GraphCopier[Field, Node, Edge] {
      override def copyNode(n: Node): Node = n match {
        case VNode(ref) =>
          n
        case LNode(fromNode, via, pPoint, types) =>
          LNode(copyNode(fromNode), copyField(via), pPoint, copyTypes(types))
        case LVNode(ref, types) =>
          LVNode(copyRef(ref), copyTypes(types))
        case INode(pPoint, sgt, types) =>
          INode(pPoint, sgt, copyTypes(types))
        case OBNode(sym) =>
          n
        case GBNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | TrueLitNode | FalseLitNode =>
          n
        case _ =>
          sys.error("Unnexpected node type at this point")
      }

      def copyRef(r: CFG.Ref): CFG.Ref = r

      def copyIEdge(ie: IEdge): IEdge =
          IEdge(copyNode(ie.v1), copyField(ie.label), copyNode(ie.v2))

      def copyOEdge(oe: OEdge): OEdge =
          OEdge(copyNode(oe.v1), copyField(oe.label), copyNode(oe.v2))

      override def copyEdge(e: Edge): Edge = e match {
        case ie: IEdge =>
          copyIEdge(ie)
        case oe: OEdge =>
          copyOEdge(oe)
        case _ =>
          sys.error("Unnexpected edge type at this point")
      }

      def copyField(f: Field): Field = f

      def copyTypes(oset: ObjectSet): ObjectSet = oset

      def copyTypesWithMap(map: Map[Type, Set[Type]])(oset: ObjectSet): ObjectSet = {
        oset
      }
    }

  }
}
