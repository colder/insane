class Counter {
  var i: Int = 0

  def inc {
    i += 1
  }
}

class A {
  var b: Counter = new Counter

  def test(a: Counter) {
    a.inc
    b.inc
    val a2 = a
    a2.inc
    a2
  }
}
