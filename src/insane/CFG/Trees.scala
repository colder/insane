package insane
package CFG

import utils.ASTBindings

import scala.tools.nsc.symtab._

trait Trees extends ASTBindings {
  self: AnalysisComponent =>

  private var _nextID = 0;

  import global.{Name, Symbol}

  def nextID = {
    _nextID += 1;
    _nextID
  }

  sealed abstract class Tree extends ASTBound {
    val uniqueID = nextID
  }

  sealed abstract class Statement extends Tree

  case class AssignVal(val r: Ref, val v: SimpleValue)                                                          extends Statement
  case class AssignSelect(val r: Ref, val obj: SimpleValue, val field: FieldRef)                                extends Statement
  case class AssignApply(val r: Ref, val receiver: SimpleValue, val method: Symbol, val args: Seq[SimpleValue]) extends Statement
  case class AssignNew(val r: Ref, val cl: ClassRef, val args: Seq[Seq[SimpleValue]])                           extends Statement

  case class Assert(val v: SimpleValue) extends Statement

  case class Branch(val cond: BranchCondition) extends Statement
  case class Skip() extends Statement


  sealed abstract class SimpleValue     extends Statement

  case class Ref(sym: Symbol)           extends SimpleValue
  case class This(n: Name)              extends SimpleValue
  case class Super(n: Name, mix: Name)  extends SimpleValue


  sealed abstract class StaticValue     extends SimpleValue

  case class Long(value: Long)          extends StaticValue
  case class Float(value: Float)        extends StaticValue
  case class String(value: String)      extends StaticValue
  case class True()                     extends StaticValue
  case class Any()                      extends StaticValue
  case class False()                    extends StaticValue
  case class Null()                     extends StaticValue

  sealed abstract class BranchCondition extends Tree
  case object Maybe extends BranchCondition

  case class ClassRef(val n: Name) extends Tree
  case class FieldRef(val n: Name) extends Tree
}
