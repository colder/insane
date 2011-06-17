class A {
  def test(a: A) {
    Obj.foo = a
  }

  def plop = {
    val a = new A

    test(a)
  }
}

object Obj {
  var foo: A = null
}
