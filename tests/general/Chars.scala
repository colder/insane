class A {
  def test(s: String) {
    val c = if (s contains '"') '\'' else '"'
    c + s + c
  }
}
