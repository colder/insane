class A {
  var f: A = null
  def test(a1: A, a2: A, a3: A) {
    a2.f = this.f
    a1.f = a2
    a1.f = a3
  }

  def plop() {
    val o1 = new A
    val o2 = new A
    val o3 = if (this!=null) o1 else o2
    o1.test(o3, o2, o2)
  }
}
