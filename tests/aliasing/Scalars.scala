class A(var i: Int)

object Main {
  def test1() {
    val obj = testScalar(3)
  }

  def testScalar(a: Int): A = {
    new A(a)
  }
}
