class LList(var data: Int,
            var next: LList)

object Test {
  def run1(ll1: LList,
           ll2: LList): LList = {
    val old = ll1.next
    ll1.next = ll2
    old
  }

  def run2() = {
    val ll = new LList(0,
              new LList(1,
               new LList(2, null)))

    run1(ll, ll)
  }
}
