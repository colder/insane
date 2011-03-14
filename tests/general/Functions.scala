package tests

class Functions {
  def b(a: Int = 2) = a + 1

  private[this] var field = (x: Int) => x + 2;

  def a() {
    def plop(p: Int) = p + 5


    val c = b(_)

    field = c;

    c(2)

    b(2)

    plop(3)

    val d = (a: Int) => a + 4

    val f = d

    f(3)
    d(4)
  }

}
