class LList(var data: Int,
            var next: LList)

object Test {
  def run(ll1: LList,
           ll2: LList): LList = {
    val old = ll1.next
    ll1.next = ll2
    old
  }

  def r1() = {
    val ll = new LList(0,
              new LList(1,
               null))
    run(ll, ll)
    ll
  }

  def r2() = {
    val ll = new LList(0,
              new LList(1,
               null))

    ll
  }
}
