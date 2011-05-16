class A {
  var f: A = null

  def load_noescape() {
    val v1 = new A
    val v2 = v1.f
  }

  def load_escape(v1: A) {
    val v2 = v1.f
  }
}
