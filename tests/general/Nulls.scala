object Test {
  var a: A = null

  def test() {
    if (null != a) {
      println("plop")
    }
  }
}

class A {}
