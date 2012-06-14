import insane.annotations.AssertUntouched

class El(var a: Int, var b: Int)

object Test3 {
  @AssertUntouched("l.tl*.hd.b")
  def f1(l: List[El]) {
    for (v <- l) {
      v.a = 42;
    }
  }
  def f2(l: List[El]) {
    for (v <- l) {
      v.b = 42;
    }
  }
}
