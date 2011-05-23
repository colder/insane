class A {
  var next: A = null
}
object Recency {
  def test() {
    val a = new A
    val b = new A
    a.next = b

    var as = new A
    while(a.next != null) {
      as = new A
    }

    as.next = a.next
  }
}
