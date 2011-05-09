package tests.aliasing

class A;

class Cell(val data: A) {
  var next: Cell = null
}

object Test {
  final var a: Cell = new Cell(new A)
}

class Aliasing005() {
  private[this] var cell = new Cell(new A)

  def next(): A = {
    val a = this.cell

    val res = a.data

    this.cell = a.next

    res
  }
}
