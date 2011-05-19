class A(val next: A)
object Foo {

  def test() {
    val n = new A(null)
    val a = new A(n)

    assert(plop(a) eq n)
  }

  def plop(a: A) = {
    a.next
  }

}
