class LList(var data: Int, var next: LList) {
  def mapHead(f: F1): LList = {
    new LList(f.apply(data), next)
  }
}

class Test {
  var v: Int = _

  def test(l: LList) = {
    l.mapHead(new F1_1(this))
  }
}
abstract class F1 {
  def apply(i: Int): Int = 2
}

class F1_1(t: Test) extends F1 {
  override def apply(i: Int): Int = { t.v = i; 42 }
}
class F1_2 extends F1 {
  override def apply(i: Int): Int = 2
}
class F1_3 extends F1 {
  override def apply(i: Int): Int = 2
}
class F1_4 extends F1 {
  override def apply(i: Int): Int = 2
}
