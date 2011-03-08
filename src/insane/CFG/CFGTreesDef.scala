package insane
package CFG

import utils.ASTBindings

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

    class This(n: Name)              extends SimpleValue
    class Super(n: Name, mix: Name)  extends SimpleValue

    sealed abstract class LiteralValue extends SimpleValue

    class StringLit(v: String)     extends LiteralValue
    class IntLit(v: Int)           extends LiteralValue
    class FloatLit(v: Double)      extends LiteralValue
    class Unit                     extends LiteralValue


    sealed abstract class BranchCondition extends Tree
    class IfTrue(sv: SimpleValue) extends BranchCondition
    class IfFalse(sv: SimpleValue) extends BranchCondition

    class ClassRef(val n: Name) extends Tree
    class FieldRef(val n: Name) extends Tree
  }
}
