class Counter {
  var v: Int = 0
}

class Test1 {
  def fun() {
    val localC = new Counter
    localC.v = 42
  }
}

class Test2 {
  val c = new Counter

  def fun() {
    c.v = 42
  }
}
