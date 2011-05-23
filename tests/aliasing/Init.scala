class A(val a: A) {
  var b: A = _
  var c: A = null


  b = a
  c = b

  def test() = {
    val tmpA = a.a
    val tmpB = a.b
    val tmpC = a.c

    a.c
  }
}

object Test {
  def main() {
    val t = new A(null)
    val a = new A(t)

    val tmpret = a.test()
  }
}
