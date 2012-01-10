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
      (this.ids.keySet ++ that.ids.keySet).forall(k => (this.ids(k) + that.ids(k)) <= settings.depthResolution)
    }

    override def toString = ids.map{ case (i, n) => i+(if(n > 1) "("+n+")" else "") }.mkString("[", ",", "]")
  }

  object NoUniqueID extends UniqueID(Map(0 -> 1).withDefaultValue(0))
}
