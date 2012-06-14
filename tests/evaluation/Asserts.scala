import insane.annotations.AssertUntouched

class El(var a: Int, var b: Int)

object Test3 {
  @AssertUntouched("a")
  def f100(l: El) {
    
  }
  @AssertUntouched("b")
  def f101(l: El) {
    
  }
  @AssertUntouched("a.b")
  def f102(l: El) {
    
  }
  @AssertUntouched("a|b")
  def f103(l: El) {
    
  }
  @AssertUntouched("(a)")
  def f104(l: El) {
    
  }
  @AssertUntouched("a*")
  def f105(l: El) {
    
  }
  @AssertUntouched(" a . b")
  def f106(l: El) {
    
  }
  @AssertUntouched("l|tl*.hd.b")
  def f1(l: El) {
    
  }
  @AssertUntouched("l.tl*.hd.b")
  def f2(l: El) {
    
  }
  @AssertUntouched("l.(tl)*.hd.b")
  def f3(l: El) {
    
  }
  @AssertUntouched("l.tl*|hd.b")
  def f4(l: El) {
    
  }
  @AssertUntouched("l.tl*|hd.b |||")
  def f12(l: El) {
    
  }
}
