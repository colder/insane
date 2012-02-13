package t8

class T {
  var t: T = null;
}

class Usage {

  def copy(t1: T, t2: T) = {
    t1.t.t = t2.t
  }
  def run(t1: T, t2: T) = {
    val t3 = new T
    t1.t = t3
    t2.t = t3

    copy(t1,t2)

  }
}


