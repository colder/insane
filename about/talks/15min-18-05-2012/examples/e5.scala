class LList(var data: Int, var next: LList) {
}

class Test {
  def f(l: LList) = {
    var c = l
    while(c.next != null) {
      c = c.next
    }
    c
  }
}
