object Pattern {
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit = xs match {
    case x: Array[AnyRef]  => x(idx) = value.asInstanceOf[AnyRef]
    case x: Array[Int]     => x(idx) = value.asInstanceOf[Int]
    case x: Array[Double]  => x(idx) = value.asInstanceOf[Double]
    case x: Array[Long]    => x(idx) = value.asInstanceOf[Long]
    case x: Array[Float]   => x(idx) = value.asInstanceOf[Float]
    case x: Array[Char]    => x(idx) = value.asInstanceOf[Char]
    case x: Array[Byte]    => x(idx) = value.asInstanceOf[Byte]
    case x: Array[Short]   => x(idx) = value.asInstanceOf[Short]
    case x: Array[Boolean] => x(idx) = value.asInstanceOf[Boolean]
    case x: Array[Unit]    => x(idx) = value.asInstanceOf[Unit]
    case null => throw new NullPointerException
  }
}
