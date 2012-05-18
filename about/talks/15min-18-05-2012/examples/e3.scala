class LList(var data: Int, var next: LList) {
  def mapHead(f: F1): LList = {
    new LList(f.apply(data), next)
  }
}

abstract class F1 {
  def apply(i: Int): Int = 2
}

class F1_1 extends F1 {
  override def apply(i: Int): Int = 2
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
