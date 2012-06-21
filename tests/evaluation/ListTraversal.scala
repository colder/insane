import insane.annotations.{WillNotModify, MayOnlyModify}

class El(var a: Int, var b: Int)

object Test3 {
  @WillNotModify("l.tl*.hd.b")
  def f1(l: List[El]) {
    for (v <- l) {
      v.a = 42;
    }
  }

  @MayOnlyModify("l.hd.b")
  def f2(l: List[El]) {
    for (v <- l) {
      v.b = 42;
    }
  }

  def f3(l: List[El], f: El => Unit) {
    l.foreach(f)
  }
}
