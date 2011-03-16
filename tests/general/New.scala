package tests.general

class New_A(val a: Int);
class New_B extends New_A(2);

class New {
  def a = {
    var t1 = new New_A(3);
    var t2 = new New_B;

    t1 = t2;
  }
}
