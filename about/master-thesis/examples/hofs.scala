object Test {
  def test() = {
    plop(43, _ + 1)
  }
  def plop(i: Int, f: Int => Int): Int = f(i)
}
