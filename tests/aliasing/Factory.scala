class A {
  def factory = new A


  def test() {
    val init = factory
    while(init != null) {
      val a = factory
      val b = factory
      val c = new A
    }
  }
}
