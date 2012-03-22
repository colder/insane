package evaluation

abstract class Value
class StringValue(str: String) extends Value
class IntValue(i: Int) extends Value
object NullValue extends Value

class Cell(var v: Value) {
  def matches(o: Value) = {
    false
  }
}

class Database(var rows: List[List[Cell]]) {
  def select(p: List[Cell] => Boolean): Database = {
    new Database(rows.filter(p))
  }

  def insert(e: List[Cell]): Unit = {
    rows = new Cons(e, rows)
  }

  def replace(p: List[Cell] => List[Cell]): Unit = {
    rows = rows.map(p)
  }

  def count(p: List[Cell] => Boolean): Int= {
    var counter = 0;
    rows.foreach({row => if (p(row)) { counter += 1 }})
    counter
  }
}

object SQLPredicates {
  val all  = {row: List[Cell] => true}
  val none = {row: List[Cell] => false}
  def isNull = {cell: Cell => cell.v == NullValue}
  def hasNull = {row: List[Cell] => row.exists(isNull)}
}

object Main {
  import SQLPredicates._

  def run(db: Database) = {
    val res = db.select(hasNull)

    val row = new Cons(new Cell(new IntValue(42)),
                new Cons(new Cell(new StringValue("foo")), Nil))

    res.insert(row)

    res
  }
}
