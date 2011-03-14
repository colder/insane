package insane
package CFG

import AST.ASTBindings

import scala.tools.nsc.Global
import scala.tools.nsc.symtab._

trait CFGTreesDef extends ASTBindings {
  val global: Global

  object CFGTrees {
    import global.{Name, Symbol}

    private var _nextID = 0;

    def nextID = {
      _nextID += 1;
      _nextID
    }

    sealed abstract class Tree extends ASTBound {
      val uniqueID = nextID

      override def toString = stringRepr(this)
    }

    sealed abstract class Statement extends Tree

    class AssignVal(val r: Ref, val v: SimpleValue)                                                          extends Statement
    class AssignSelect(val r: Ref, val obj: SimpleValue, val field: Symbol)                                  extends Statement
    class AssignApplyFun(val r: Ref, val fun: Symbol, val args: Seq[SimpleValue])                            extends Statement
    class AssignApplyMeth(val r: Ref, val obj: SimpleValue, val meth: Symbol, val args: Seq[SimpleValue])    extends Statement
    class AssignNew(val r: Ref, val cl: Symbol, val args: Seq[SimpleValue])                                  extends Statement

    class Assert(val v: SimpleValue)         extends Statement

    class Branch(val cond: BranchCondition)  extends Statement

    object Skip extends Statement


    sealed abstract class SimpleValue        extends Statement

    sealed abstract trait Ref                extends SimpleValue

    class SymRef(val symbol: Symbol)         extends Ref
    class TempRef(val name: String)          extends Ref

    class AnnonFun(val symbol: Symbol)       extends SimpleValue

    class This(val n: Name)                  extends SimpleValue
    class Super(val n: Name, val mix: Name)  extends SimpleValue

    sealed abstract class LiteralValue extends SimpleValue

    class StringLit(val v: String)     extends LiteralValue
    class BooleanLit(val v: Boolean)   extends LiteralValue
    class LongLit(val v: Long)         extends LiteralValue
    class DoubleLit(val v: Double)     extends LiteralValue
    class Unit                         extends LiteralValue


    sealed abstract class BranchCondition extends Tree
    class IfTrue(val sv: SimpleValue) extends BranchCondition
    class IfFalse(val sv: SimpleValue) extends BranchCondition


    def stringRepr(tr: Tree): String = tr match {
      case t: AssignVal =>
        stringRepr(t.r) +" = "+stringRepr(t.v)
      case t: AssignSelect =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.field.name
      case t: AssignApplyFun =>
        stringRepr(t.r) +" = "+t.fun.name.toString+t.args.map(stringRepr).mkString("(", ", ", ")")
      case t: AssignApplyMeth =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.meth.name+t.args.map(stringRepr).mkString("(", ", ", ")")
      case t: AssignNew =>
        stringRepr(t.r) +" = new "+t.cl.name+t.args.map(stringRepr).mkString("(", ", ", ")");
      case t: Assert =>
        "assert("+stringRepr(t.v)+")"
      case t: Branch =>
        "["+stringRepr(t.cond)+"]"
      case t: IfTrue =>
        stringRepr(t.sv)
      case t: IfFalse =>
        "!"+stringRepr(t.sv)
      case Skip =>
        "skip"
      case r: SymRef =>
        r.symbol.toString
      case r: TempRef =>
        r.name
      case f: AnnonFun =>
        f.symbol.toString
      case t: This =>
        "this["+t.n.toString+"]"
      case t: Super =>
        "super["+t.n.toString+", "+t.mix.toString+"]"
      case t: StringLit =>
        "\""+t.v+"\""
      case t: LongLit =>
        t.v.toString
      case t: DoubleLit =>
        t.v.toString
      case t: Unit =>
        "unit"
      case t: BooleanLit =>
        t.v.toString
    }
  }

}
