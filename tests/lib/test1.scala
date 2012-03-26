package t1

class Counter(var v: Int)


class Test {
  def myMethod() {
    val c = new Counter(0)

    c.v = 1
  }

  def test(c: Counter) {
    c.v = 1
  }
}
