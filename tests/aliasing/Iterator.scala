class Data(var value: Int) {

}

class Cell {
  var data: Data = null
  var next: Cell = null
}

class MyList(val head: Cell) {
  def iterator = new Iter(head)
}

object Main {
  def sum(l: MyList): Int = {
    val i = l.iterator
    var res = 0
    while (i.hasNext) {
      val d = i.next
      res += d.value
    }

    assert(i.cell ne l.head)

    res
  }

}

class Iter(var cell: Cell) {
  def next: Data = {
    val c = this.cell
    this.cell = c.next
    c.data
  }

  def hasNext = {
    cell != null
  }
}


