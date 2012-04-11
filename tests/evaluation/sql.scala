abstract class F1[-T1, +R] {
  def apply(a1: T1): R
}

abstract class F2[-T1, -T2, +R] {
  def apply(a1: T1, a2: T2): R
}

abstract class List[+T] {
  def forall(f: F1[T, Boolean]): Boolean
  def exists(f: F1[T, Boolean]): Boolean
  def filter(f: F1[T, Boolean]): List[T]
  def foreach(f: F1[T, Unit]): Unit
  def map[B](f: F1[T, B]): List[B]
  def size: Int
}

class Counter(var v: Int) {
  def inc {
    inc(1)
  }

  def inc(inc: Int) {
    v = v + inc
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def forall(f: F1[T, Boolean]): Boolean =
    f.apply(head) && tail.forall(f)

  def exists(f: F1[T, Boolean]): Boolean =
    f.apply(head) || tail.exists(f)

  def filter(f: F1[T, Boolean]): List[T] =
    if (f(head)) {
      new Cons[T](head, tail.filter(f))
    } else {
      tail.filter(f)
    }


  def foreach(f: F1[T, Unit]): Unit = {
    f.apply(head)
    tail.foreach(f)
  }

  def size: Int = {
    1 + tail.size
  }

  def map[B](f: F1[T, B]): List[B] =
    new Cons[B](f.apply(head), tail.map(f))
}

object Nil extends List[Nothing] {
  def forall(t: F1[Nothing, Boolean]): Boolean =
    true
  def exists(t: F1[Nothing, Boolean]): Boolean =
    false
  def foreach(t: F1[Nothing, Unit]): Unit =
    {}
  def map[B](t: F1[Nothing, B]): List[B] =
    Nil

  def filter(t: F1[Nothing, Boolean]): List[Nothing] =
    Nil

  def size: Int = 0
}

class IntValue(var i: Int);

class Cell(var v: IntValue) {
  def matches(o: IntValue) = {
    false
  }
}

class Database(var rows: List[List[Cell]]) {
  def select(p: F1[List[Cell], Boolean]): Database = {
    new Database(rows.filter(p))
  }

  def insert(e: List[Cell]): Unit = {
    rows = new Cons(e, rows)
  }

  def replace(p: F1[List[Cell], List[Cell]]): Unit = {
    rows = rows.map(p)
  }

  def count(p: F1[List[Cell], Boolean]): Int= {
    val c = new Counter(0)
    select(p).foreach(new F1_Counter_Inc(c))
    c.v
  }

  def countAll: Int= {
    val c = new Counter(0)
    foreach(new F1_Counter_Inc(c))
    c.v
  }

  def foreach(p: F1[List[Cell], Unit]): Unit = {
    rows.foreach(p)
  }
}

class F1_Counter_Inc(c: Counter) extends F1[List[Cell], Unit] {
  def apply(row: List[Cell]): Unit = {
    c.inc
  }
}

class F1_Average(var c: Counter, var n: Counter) extends F1[List[Cell], Unit] {
  def apply(row: List[Cell]): Unit = {
    row match {
      case cons: Cons[_] =>
        c.inc(cons.head.v.i)
        n.inc
      case _ =>
    }
  }
}

class F1_AllTrue extends F1[List[Cell], Boolean] {
  def apply(row: List[Cell]): Boolean = {
    true
  }
}

class F1_AllFalse extends F1[List[Cell], Boolean] {
  def apply(row: List[Cell]): Boolean = {
    false
  }
}

object Test {

  def run1(db: Database) = {
    db.countAll
  }

  def run2(db: Database) = {
    db.count(new F1_AllTrue)
  }

  def run3(db: Database) = {
    new F1_Counter_Inc(new Counter(0))
  }
}
