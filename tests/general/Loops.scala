package tests.general

class Statements {
  def a() = {

      var f = 2

      while (f < 3) {
        f += 4
      }

      do {
        f += 4
      } while ( f < 10)

      f = 5;

  }

  def funLoop() = {
    def loop(s: Statements): Boolean = s != null || loop(s)
    loop(this)
  }
}
