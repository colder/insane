import insane.annotations.{WillNotModify, MayOnlyModify}

class El(var a: Int, var b: Int)

object Test3 {
  @WillNotModify("l.tl*.hd.b")
  @MayOnlyModify("l.tl*.hd.a")
  def f1(l: List[El]) {
    for (v <- l) {
      v.a = 42;
    }
  }

  @WillNotModify("l.tl*.hd.a")
  @MayOnlyModify("l.tl*.hd.b")
  def f2(l: List[El]) {
    for (v <- l) {
      v.b = 42;
    }
  }
}
