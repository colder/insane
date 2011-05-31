class A {

  var next:  A = null

  def test() = {
    var a = new A

    while(a != null) {
      a = a.next
    }

    a
  }

  def multiRead(a: A) = {
    if (a != null) {
      a.next
    } else {
      a.next
    }
  }

  def useMultiRead = {
    val a = new A

    val b = multiRead(a)

    b
  }


  def branches(o1: A, o2: A) = {
    val a = new A

    if (a != null) {
      a.next = o1
    }

    if (a != null) {
      a.next = o2
    }
  }

  def useBranches() = {
    val o1 = new A
    val o2 = new A

    branches(o1, o2)
  }



  def problem1(a: A, b: A) = {
    val o = if (a != null) a else b

    o.next = b
  }

  def problem2(a: A) = {
    problem2in(a, a)
  }

  def problem2in(a: A, b: A) = {
    a.next = new A
    b.next = new A
  }
}
