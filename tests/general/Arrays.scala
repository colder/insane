class A {
  def plop() = {

  }
}

object Plop {
  var a: Array[Int] = Array[Int](1,2,3)
  var aar: Array[A] = Array[A](new A, new A)

  def test() {
    println(a(2))
    a(3) = 1;
  }

  def test2() {
    val a = aar(1)

    a.plop()
  }
}
