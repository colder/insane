class A(var f: A) {
  def test(a1: A, a2: A) {
    val o = if (a1 != a2) a1 else a2
    a1.f = null
    o.f = null
  }
}
