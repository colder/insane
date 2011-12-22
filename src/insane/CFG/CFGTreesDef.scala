package insane
package CFG

import AST.ASTBindings

import scala.tools.nsc._
import utils._
import utils.Graphs.DotConverter

trait CFGTreesDef extends ASTBindings { self: AnalysisComponent =>
  val global: Global

  object CFGTrees {
    import global._

    private var _nextID = 0;

    def nextID = {
      _nextID += 1;

      new UniqueID(_nextID)
    }

    /*
    private var _version = 0;
    */

    /*
    def nextVersion = {
      _version += 1

      _version
    }
    */

    sealed abstract class Tree extends ASTBound {
      val uniqueID = nextID

      lazy val pos = getTree.pos

      override def toString = stringRepr(this)
    }

    sealed abstract class Statement extends Tree

    class AssignCast(val r: Ref, val rhs: Ref, val tpe: Type)                      extends Statement
    class AssignTypeCheck(val r: Ref, val lhs: Ref, val tpe: Type)                 extends Statement
    class AssignVal(val r: Ref, val v: SimpleValue)                                extends Statement
    class AssignFieldRead(val r: Ref, val obj: Ref, val field: Symbol)             extends Statement
    class AssignFieldWrite(val obj: Ref, val field: Symbol, val rhs: SimpleValue)  extends Statement
    class AssignNew(val r: Ref, val tpe: Type)                                     extends Statement
    class AssignApplyMeth(val r: Ref, val obj: SimpleValue, val meth: Symbol, val args: Seq[SimpleValue], val isDynamic: Boolean) extends Statement
    class AssertEQ(val lhs: SimpleValue, val rhs: SimpleValue)                     extends Statement
    class AssertNE(val lhs: SimpleValue, val rhs: SimpleValue)                     extends Statement
    class Branch(val cond: BranchCondition)                                        extends Statement
    class Effect(val env: PTEnv, val name: String)                                 extends Statement
    object Skip                                                                    extends Statement


    sealed abstract class SimpleValue extends Tree

    sealed abstract class Ref extends SimpleValue {
      def tpe: Type;
    }

    sealed trait TypedSymbolRef {
      this: {val symbol: Symbol} =>

      def tpe = symbol.tpe;
    }

    case class ObjRef(symbol: Symbol)                 extends Ref with TypedSymbolRef
    case class SymRef(symbol: Symbol, version: UniqueID)   extends Ref with TypedSymbolRef
    case class TempRef(name: String, version: UniqueID, tpe: Type)       extends Ref
    case class SuperRef(symbol: Symbol, version: UniqueID) extends Ref with TypedSymbolRef

    // Mutable only during CFG Generation
    case class ThisRef(var symbol: Symbol, version: UniqueID) extends Ref with TypedSymbolRef

    class Null extends SimpleValue

    sealed abstract class LiteralValue extends SimpleValue

    class StringLit(val v: String)     extends LiteralValue
    class BooleanLit(val v: Boolean)   extends LiteralValue
    class LongLit(val v: Long)         extends LiteralValue
    class IntLit(val v: Int)           extends LiteralValue
    class ShortLit(val v: Short)       extends LiteralValue
    class ByteLit(val v: Byte)         extends LiteralValue
    class CharLit(val v: Char)         extends LiteralValue
    class FloatLit(val v: Float)       extends LiteralValue
    class DoubleLit(val v: Double)     extends LiteralValue
    class ClassLit(val tpe: Type)      extends LiteralValue
    class EnumLit(val tpe: Type)       extends LiteralValue
    class Unit                         extends LiteralValue


    sealed abstract class BranchCondition extends Tree
    class IfTrue(val sv: SimpleValue) extends BranchCondition
    class IfFalse(val sv: SimpleValue) extends BranchCondition
    class IfEqual(val rhs: SimpleValue, val lhs: SimpleValue) extends BranchCondition
    class IfNotEqual(val rhs: SimpleValue, val lhs: SimpleValue) extends BranchCondition


    def stringRepr(tr: Tree): String = tr match {
      case t: AssignCast =>
        stringRepr(t.r) +" = "+stringRepr(t.rhs)+".$asInstanceOf["+t.tpe.toString+"]"
      case t: AssignTypeCheck =>
        stringRepr(t.r) +" = "+stringRepr(t.lhs)+".$isInstanceOf["+t.tpe.toString+"]"
      case t: AssignVal =>
        stringRepr(t.r) +" = "+stringRepr(t.v)
      case t: AssignFieldRead =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.field.name
      case t: AssignFieldWrite =>
        stringRepr(t.obj) +"."+t.field.name+" = "+stringRepr(t.rhs)
      case t: AssignApplyMeth =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.meth.name+(if(t.isDynamic) "@dyn" else "")+t.args.map(stringRepr).mkString("(", ", ", ")")
      case t: AssignNew =>
        stringRepr(t.r) +" = new "+t.tpe
      case t: AssertEQ =>
        "assert("+stringRepr(t.lhs)+" eq "+stringRepr(t.rhs)+")"
      case t: AssertNE =>
        "assert("+stringRepr(t.lhs)+" ne "+stringRepr(t.rhs)+")"
      case t: Branch =>
        "["+stringRepr(t.cond)+"]"
      case Skip =>
        "skip"
      case r: ObjRef =>
        r.symbol.name.toString()
      case r: SymRef =>
        if (r.version != NoUniqueID) r.symbol.name.toString()+"@"+r.version else r.symbol.name.toString()
      case r: TempRef =>
        if (r.version != NoUniqueID) r.name+"@"+r.version else r.name
      case r: ThisRef =>
        if (r.version != NoUniqueID) "this@"+r.version else "this"
      case r: SuperRef =>
        if (r.version != NoUniqueID) r.symbol.name+".super@"+r.version else r.symbol.name+".super"
      case t: StringLit =>
        "\""+t.v+"\""
      case t: ByteLit =>
        t.v.toString
      case t: ShortLit =>
        t.v.toString
      case t: CharLit =>
        t.v.toString
      case t: IntLit =>
        t.v.toString
      case t: FloatLit =>
        t.v.toString
      case t: LongLit =>
        t.v.toString
      case t: ClassLit =>
        "classOf["+t.tpe+"]"
      case t: EnumLit =>
        "enum?"
      case t: DoubleLit =>
        t.v.toString
      case n: Null =>
        "null"
      case t: Unit =>
        "unit"
      case t: BooleanLit =>
        t.v.toString
      case e: Effect =>
        "some effect"
      case t: IfTrue =>
        stringRepr(t.sv)
      case t: IfFalse =>
        "!"+stringRepr(t.sv)
      case t: IfEqual =>
        stringRepr(t.lhs)+" == "+stringRepr(t.rhs)
      case t: IfNotEqual =>
        stringRepr(t.lhs)+" != "+stringRepr(t.rhs)
    }
  }



  class CFGDotConverter(cfg: FunctionCFG, _title: String, _prefix: String = "") extends DotConverter(cfg.graph, _title, _prefix) {
    import utils.DotHelpers

    override def vertexToString(res: StringBuffer, v: CFGVertex) {
        if (v == cfg.entry) {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name)+"\", style=filled, color=\"green\"];\n")
        } else if (v == cfg.exit) {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name)+"\", style=filled, color=\"red\"];\n")
        } else {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name+"#"+v.id)+"\"];\n")
        }
    }
    override def edgeToString(res: StringBuffer, le: CFGEdge[CFGTrees.Statement]) {

      le.label match {
        case e: CFGTrees.Effect =>
          val id = e.uniqueID.ids.mkString("")

          val clusterName = "cluster"+id;

          res append "subgraph "+clusterName+" {\n"
          res append "  label=\""+DotHelpers.escape(e.name)+"\";\n"
          res append "  color=\"gray\";\n"

          if (e.env.isBottom) {
            res append "  bottom"+id+" [label=\"(Bottom)\"]; "
          } else {
            val ptdot = new PointToGraphs.PTDotConverter(e.env, "Effects", "x"+id+prefix)
            ptdot.drawGraph(res)
          }

          res append "}\n"

          res append DotHelpers.arrow(le.v1.dotName, clusterName)
          res append DotHelpers.arrow(clusterName, le.v2.dotName)
        case _ =>
          res append DotHelpers.arrow(le.v1.dotName, le.dotName)
          res append DotHelpers.arrow(le.dotName, le.v2.dotName)
          res append DotHelpers.box(le.dotName, le.label.toString)
      }
    }
  }
}

