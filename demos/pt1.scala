class T(var f1: T, var f2: T) {
  def test() = {
    var tmp = this.f2
    if (tmp != null) {
      tmp = new T(null, null)
    }
    this.f1 = tmp
    tmp
  }
}

