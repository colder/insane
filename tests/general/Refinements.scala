class A {

}


object Test {
  def test() = {
    val a = new A {
      def innexistent() = 2
    }

    var b = 0
    for (i <- 1 to 10) {
      b += a.innexistent
    }
  }
}
