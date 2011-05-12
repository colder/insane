class A {
  private var f: A = null


  def load_noescape() {
    val v1 = new A
    val v2 = v1.f
  }

  def load_escape(v1: A) {
    val v2 = v1.f
  }

  def copy() {
    var v1 = new A
    var v2 = new A

    v1 = v2
  }

  def neww() {
    var v1 = new A

    v1 = new A
  }

  def store() {
    var v1 = new A
    var v2 = new A

    v1.f = new A
    v1.f = v2
  }
}
