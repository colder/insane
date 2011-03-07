package insane.CFG


object Trees {

  private var _nextID = 0;

  def nextID = {
    _nextID += 1;
    _nextID
  }

  sealed abstract class Tree extends ASTBound {
    val uniqueID = nextID
  }

  sealed abstract class Statement extends Tree

  case class AssignVal(val r: Ref, val v: SimpleValue) extends Statement
  case class AssignMethodCall(val r: Ref, val receiver: SimpleValue, val method: MethodRef, val args: Seq[Seq[SimpleValue]]) extends Statement
  case class AssignNew(val r: Ref, val f: ClassRef, val args: Seq[SimpleValue]) extends Statement

  case class Assert(val v: SimpleValue) extends Statement

  case class Branch(val cond: BranchCondition) extends Statement
  case class Skip() extends Statement

  sealed abstract class SimpleValue extends Statement
  // TODO Ref, This?

  sealed abstract class StaticValue extends SimpleValue
  case class Long(value: Long) extends StaticValue
  case class Float(value: Float) extends StaticValue
  case class String(value: String) extends StaticValue
  case class True() extends StaticValue
  case class Any() extends StaticValue
  case class False() extends StaticValue
  case class Null() extends StaticValue

  sealed abstract class BranchCondition extends Tree
  case object Maybe extends BranchCondition

  // TODO: ClassRef, MethodRef
}
