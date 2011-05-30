class A {
  def test() {
    val a = new B

    val b = a.test2(a)
    val c = b.test2(a)
    val d = c.test2(a)
  }

  def test2(a: A): A = {
    a
  }
}

class B extends A {
  override def test2(a: A): A = {
    a
  }
}
