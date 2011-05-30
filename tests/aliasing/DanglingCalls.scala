class A {
  var value: A = null

  def test() {
    val a = new B

    testNotInlined(a)
  }

  def testNotInlined(a: A): A = {
    a.test2(a)
  }

  def test2(a: A): A = {
    a.value
  }
}

class B extends A {
  override def test2(a: A): A = {
    a.value = a
    a.value
  }
}
