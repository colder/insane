object Plop {

  def plop = (new A, new A, new A)

  def foo = {
    val (a,b,c) = plop

    a.test
  }
}

class A {
  def test = 2
}
