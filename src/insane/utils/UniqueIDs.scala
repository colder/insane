package insane
package utils

trait UniqueIDs {
  self : AnalysisComponent =>

  case class UniqueID(ids: Map[Int, Int]) {
    def this(i: Int) = this(Map(i -> 1).withDefaultValue(0))

    def add(that: UniqueID) = {
      var map = ids

      for ((i, n) <- that.ids) {
        map = map + (i -> (map(i)+n))
      }

      UniqueID(map)
    }

    def safeAdd(that: UniqueID) = {
      if (safeToAdd(that)) {
        add(that)
      } else {
        this
      }
    }

    private def safeToAdd(that: UniqueID) = {
      (this.ids.keySet ++ that.ids.keySet).forall(k => (this.ids(k) + that.ids(k)) < settings.depthResolution)
    }

    override def toString = ids.map{ case (i, n) => i+(if(n > 1) "("+n+")" else "") }.mkString("[", ",", "]")
  }

  object NoUniqueID extends UniqueID(Map(0 -> 1).withDefaultValue(0))

  implicit object UniqueIDOrdering extends Ordering[UniqueID] {
    def compare(x: UniqueID, y: UniqueID): Int = {
      for (k <- (x.ids.keySet ++ y.ids.keySet).toList.sorted) {
        val xv = x.ids.getOrElse(k, 0)
        val yv = y.ids.getOrElse(k, 0)

        if (xv < yv) return -1;
        if (xv > yv) return 1;
      }

      0
    }
  }
}
