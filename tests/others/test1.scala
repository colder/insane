package t1

class Counter(var v: Int)


class Test {
  def myMethod() = {
    val c = new Counter(0)
    myMethod2(c)
    c
  }

  def myMethod2(c: Counter) {
    c.v = 2
  }
}
