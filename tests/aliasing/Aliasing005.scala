package tests.aliasing

class A;

class Cell(val data: A) {
  var next: Cell = null
}

object Test {
  final var a: Cell = new Cell(new A)
}

class Aliasing005() {
  private[this] var cell  = new Cell(new A)
  private[this] var cell2 = new Cell(new A)

  def nextMeth(): A = {
    val a = this.cell

    val res = a.data

    this.cell = a.next

    this.cell2 = rec1(this.cell)

    res
  }

  def rec1(c: Cell): Cell = {
    rec2(c)
  }

  def rec2(c: Cell): Cell = {
    if (c.next == null) {
      c
    } else {
      rec1(c.next)
    }
  }
}
