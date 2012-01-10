class A (val next: A*)

object Test {
  def test() = {
    val a = new A()
    val b = new A(a)
  }
}
