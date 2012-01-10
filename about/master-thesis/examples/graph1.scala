class A {
  var f1: A = null
  var f2: A = null

  def test() = {
    val obj = new A
    val tmp = if(obj != null) {
      this.f1
    } else {
      obj
    }
  }
}
