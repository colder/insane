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
      val types: TypeInfo
      val isResolved: Boolean

      def withTypes(tpe: TypeInfo): Node
    }

    case class VNode(ref: CFG.Ref) extends Node(""+ref.toString+"", false) {
      val types = TypeInfo.empty
      val isResolved = true

      def withTypes(tpe: TypeInfo) = sys.error("VNode.withTypes()")
    }

    trait GloballyReachableNode {
      val isResolved = true
      def withTypes(tpe: TypeInfo) = sys.error(this+".withTypes()")
    }

    case class LVNode(ref: CFG.Ref, types: TypeInfo) extends Node("Loc("+ref+")", true) {
      val isResolved = false

      def withTypes(tpe: TypeInfo) = LVNode(ref, tpe)
    }
    case class INode(pPoint: UniqueID, sgt: Boolean, sym: Symbol) extends Node(sym.name+"@"+pPoint, sgt) {
      val types = TypeInfo.exact(sym.tpe)
      val isResolved = true

      def withTypes(tpe: TypeInfo) = sys.error("INode.withTypes()")
    }

    // mutable fromNode is only used when unserializing
    case class LNode(var fromNode: Node, via: Field, pPoint: UniqueID, types: TypeInfo) extends Node("L"+pPoint, true) {
      val isResolved = false

      def withTypes(tpe: TypeInfo) = LNode(fromNode, via, pPoint, tpe)
    }

    case class OBNode(s: Symbol) extends Node("Obj("+s.name+")", true) with GloballyReachableNode {
      val types = TypeInfo.exact(s.tpe)
    }

    /*
    def findSimilarLNodes(lNode: LNode, others: Set[Node]): Set[LNode] = {
      // No need to have more than one lNode with exactly the same type
      var foundExactMatch = false
      val res = others.collect {
        case l: LNode if (l != lNode) &&
                         (l.fromNode, l.via, l.pPoint) == (lNode.fromNode, lNode.via, lNode.pPoint) &&
                         (l.types isMorePreciseThan lNode.types) =>

         if (l.types == lNode.types) {
          foundExactMatch = true
         }
         l
      }

      if (foundExactMatch) {
        res
      } else {
        Set(lNode) ++ res
      }
    }
    */

    def safeLNode(from: Node, via: Field, pPoint: UniqueID): Option[LNode] = {
      val tpe = from.types.tpe

      val s = tpe.decl(via.name)

      if (s == NoSymbol) {
        //reporter.debug(t+".decl("+via.name+") == NoSymbol") 
        None
      } else {
        val realTpe = tpe.memberType(s)
        Some(safeTypedLNode(TypeInfo.subtypeOf(realTpe), from, via, pPoint))
      }
    }


    def safeTypedLNode(types: TypeInfo, from: Node, via: Field, pPoint: UniqueID): LNode = {
      LNode(from match { case LNode(lfrom, _, _, _) => lfrom case _ => from }, via, pPoint, types)
    }

    case object GBNode extends Node("Ngb", false) with GloballyReachableNode {
      val types = TypeInfo.subtypeOf(definitions.ObjectClass.tpe)
    }

    case object NNode extends Node("Null", true) with GloballyReachableNode {
      val types = TypeInfo.empty
    }

    case object UNode extends Node("Unit", true) with GloballyReachableNode {
      val types = TypeInfo.empty
    }

    case object StringLitNode extends Node("StringLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.StringClass.tpe)
    }
    case object LongLitNode extends Node("LongLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.LongClass.tpe)
    }
    case object IntLitNode extends Node("IntLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.IntClass.tpe)
    }
    case object FloatLitNode extends Node("FloatLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.FloatClass.tpe)
    }
    case object ByteLitNode extends Node("ByteLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.ByteClass.tpe)
    }
    case object CharLitNode extends Node("CharLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.CharClass.tpe)
    }
    case object ShortLitNode extends Node("ShortLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.ShortClass.tpe)
    }
    case object DoubleLitNode extends Node("DoubleLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.DoubleClass.tpe)
    }
    case object BooleanLitNode extends Node("BooleanLit", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.BooleanClass.tpe)
    }
    case object TrueLitNode extends Node("True", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.BooleanClass.tpe)
    }
    case object FalseLitNode extends Node("False", true) with GloballyReachableNode {
      val types = TypeInfo.exact(definitions.BooleanClass.tpe)
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
      } else if (t == definitions.UnitClass.tpe) {
        UNode
      } else {
        NNode
      }

    def buildPureEffect(sym: Symbol): FunctionCFG = {
      val (args, argsTypes, retval) = sym.tpe match {
        case MethodType(argssym, tpe) =>
          (argssym.map(s => new CFGTrees.SymRef(s, NoUniqueID)), argssym.map(s => TypeInfo.subtypeOf(s.tpe)), new CFGTrees.TempRef("retval", NoUniqueID, tpe))

        case tpe =>
          (Seq(), Seq(), new CFGTrees.TempRef("retval", NoUniqueID, tpe))
      }

      var cfg = new FunctionCFG(sym, args, retval, true)

      var baseEnv    = new PTEnv()

      // 1) We add 'this'/'super'
      val thisNode = LVNode(cfg.mainThisRef, TypeInfo.subtypeOf(cfg.mainThisRef.tpe))
      baseEnv = baseEnv.addNode(thisNode).setL(cfg.mainThisRef, Set(thisNode))

      // 2) We add arguments
      for ((a, info) <- cfg.args zip argsTypes) {
        val aNode = if (isGroundTypeInfo(info)) {
            typeToLitNode(info.tpe)
          } else {
            LVNode(a, info)
          }
        baseEnv = baseEnv.addNode(aNode).setL(a, Set(aNode))
      }

      // 3) return value
      val retInfo = TypeInfo.subtypeOf(retval.tpe)
      val retNode = if (isGroundTypeInfo(retInfo)) {
        typeToLitNode(retval.tpe)
      } else {
        INode(NoUniqueID, false, retval.tpe.typeSymbol)
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

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    private def completeGraph(env: PTEnv) = {
        var newGraph = env.ptGraph

        // We complete the graph with local vars -> nodes association, for clarity
        for ((ref, nodes) <- env.locState; n <- nodes) {
          newGraph += VEdge(VNode(ref), n)
        }

        newGraph
    }

    def dumpPTE(env: PTEnv, dest: String) {
      reporter.debug("Dumping Effect to "+dest+"...")
      new PTDotConverter(env, "Effect").writeFile(dest)
    }

    def dumpInlining(envInner:   PTEnv,
                     envOuter:   PTEnv,
                     envResult:  PTEnv,
                     mapInit:    Map[Node, Set[Node]],
                     mapResult:  Map[Node, Set[Node]],
                     dest:        String) {
      reporter.debug("Dumping Inlining Graphs to "+dest+"...")

      val res = new StringBuffer()

      def dumpGraph(env: PTEnv, prefix: String): PTDotConverter = {
        val clusterName = "cluster"+prefix;

        res append "subgraph "+clusterName+" {\n"
        res append "  label=\""+DotHelpers.escape(prefix)+"\";\n"
        res append "  color=\"gray\";\n"

        val ptdot = new PTDotConverter(env, "Effects", prefix)
        ptdot.drawGraph(res)

        if (env.isBottom) {
          res append "  bottom"+prefix+" [label=\"(Bottom)\", color=white]; "
        }

        res append "}\n"

        ptdot
      }

      res append "digraph D {\n"
      res append " label=\"\"\n"

      val ptIn  = dumpGraph(envInner,  "Inner")
      val ptOut = dumpGraph(envOuter,  "Outer")
      val ptRes = dumpGraph(envResult, "Result")

      for ((in, outs) <- mapInit; out <- outs) {
        res append DotHelpers.arrow(ptIn.vToS(in), ptOut.vToS(out), List("arrowhead=open", "color=red3"))
      }
      for ((in, outs) <- mapResult; out <- outs) {
        res append DotHelpers.arrow(ptIn.vToS(in), ptRes.vToS(out), List("arrowhead=open", "color=darkorange"))
      }

      res append "}\n"

      import java.io.{BufferedWriter, FileWriter}
      val out = new BufferedWriter(new FileWriter(dest))
      out.write(res.toString)
      out.close()
    }

    class PTDotConverter(_graph: PointToGraph, _title: String, _prefix: String) extends DotConverter(_graph, _title, _prefix) {
      import utils.DotHelpers

      def this(env: PTEnv, _title: String, prefix: String = "") = 
        this(completeGraph(env), _title, prefix)

      def labelToString(f: Field): String = f.strName

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
            res append DotHelpers.invisNode(vToS(v), v.name, "fontcolor=blue4" :: opts)
          case LVNode(ref, _) =>
            res append DotHelpers.dashedNode(vToS(v), v.name+"\\n"+v.types, "shape=rectangle" :: "color=green" :: opts)
          case LNode(_, _, _, _) =>
            res append DotHelpers.dashedNode(vToS(v), v.name+"\\n"+v.types, "shape=rectangle" :: opts)
          case INode(pPoint, sgt, _) =>
            res append DotHelpers.node(vToS(v), v.name+"\\n"+v.types, (if(sgt) "shape=rectangle" else "shape=box3d") ::opts)
          case GBNode | UNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | OBNode(_) | TrueLitNode | FalseLitNode =>
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
        case INode(pPoint, sgt, sym) =>
          INode(pPoint, sgt, sym)
        case OBNode(sym) =>
          n
        case GBNode | UNode | NNode | BooleanLitNode | LongLitNode | DoubleLitNode | StringLitNode | IntLitNode | ByteLitNode | CharLitNode | FloatLitNode | ShortLitNode | TrueLitNode | FalseLitNode =>
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

      def copyTypes(tpeInfo: TypeInfo): TypeInfo = tpeInfo

      def copyTypesWithMap(map: Map[Type, Set[Type]])(tpeInfo: TypeInfo): TypeInfo = {
        tpeInfo
      }
    }

  }
}
