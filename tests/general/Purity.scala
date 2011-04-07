package tests.general

class PurityCheck1 {
  var field = 2
  private[this] var field2 = 3;

  def testPure = {
    var variable = 2

    variable = 3;
  }

  def testUnpure = {
    field = 3;
  }

  def testUnpure2 = {
    field2 = 3;
  }
}
