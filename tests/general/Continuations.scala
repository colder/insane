object Plop {
   def flatMap[B](f: A => GenTraversableOnce[B]): Iterator[B] = new Iterator[B] {
    private var cur: Iterator[B] = empty
    def hasNext: Boolean =
      cur.hasNext || self.hasNext && { cur = f(self.next).toIterator; hasNext }
  } 
}
