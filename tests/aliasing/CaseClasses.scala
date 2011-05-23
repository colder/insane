case class A(var next: A) {

}

object Test {
  def test() = {
    val a = A(null)
    val b = A(a)

    val c = b.next

    val d = b.copy()
    println(d)
  }
}
