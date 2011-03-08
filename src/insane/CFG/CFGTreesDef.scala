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
    class AssignSelect(val r: Ref, val obj: SimpleValue, val field: FieldRef)                                extends Statement
    class AssignApply(val r: Ref, val receiver: SimpleValue, val method: Symbol, val args: Seq[SimpleValue]) extends Statement
    class AssignNew(val r: Ref, val cl: ClassRef, val args: Seq[Seq[SimpleValue]])                           extends Statement

    class Assert(val v: SimpleValue) extends Statement

    class Branch(val cond: BranchCondition) extends Statement

    object Skip extends Statement


    sealed abstract class SimpleValue     extends Statement

    sealed abstract class Ref        extends SimpleValue {
      val name: String
    }

    class SymRef(sym: Symbol)        extends Ref {
      val name = sym.name.toString
    }

    class TempRef(val name: String)      extends Ref

    class This(val n: Name)              extends SimpleValue
    class Super(val n: Name, val mix: Name)  extends SimpleValue

    sealed abstract class LiteralValue extends SimpleValue

    class StringLit(val v: String)     extends LiteralValue
    class IntLit(val v: Int)           extends LiteralValue
    class FloatLit(val v: Double)      extends LiteralValue
    class Unit                     extends LiteralValue


    sealed abstract class BranchCondition extends Tree
    class IfTrue(val sv: SimpleValue) extends BranchCondition
    class IfFalse(val sv: SimpleValue) extends BranchCondition

    class ClassRef(val n: Name) extends Tree
    class FieldRef(val n: Name) extends Tree

    def stringRepr(tr: Tree): String = tr match {
      case t: AssignVal =>
        stringRepr(t.r) +" = "+stringRepr(t.v)
      case t: AssignSelect =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+stringRepr(t.field)
      case t: AssignApply =>
        stringRepr(t.r) +" = "+stringRepr(t.receiver)+"."+t.method+t.args.map(stringRepr).mkString("(", ", ", ")")
      case t: AssignNew =>
        stringRepr(t.r) +" = new "+stringRepr(t.cl)+t.args.map(as => as.map(stringRepr).mkString(", ")).mkString("(", ")(", ")");

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

      case r: Ref =>
        r.name

      case t: This =>
        "this("+t.n.toString+")"

      case t: Super =>
        "super("+t.n.toString+", "+t.mix.toString+")"

      case t: StringLit =>
        "\""+t.v+"\""

      case t: IntLit =>
        t.v.toString

      case t: FloatLit =>
        t.v.toString

      case t: Unit =>
        "unit"


      case t: ClassRef =>
        t.n.toString
      case t: FieldRef =>
        t.n.toString
    }
  }

}
