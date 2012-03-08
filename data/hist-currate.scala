import scala.io.Source
import scala.collection.mutable.Map

val src = Source.fromFile("targets")

val ranges : Array[(Int,Int)] = Array(
  (0, 1),
  (2, 10),
  (11, 100),
  (101, 1000),
  (1000, Int.MaxValue)
)

val results : Map[Int,Map[Int,Int]] = Map(0 -> Map.empty[Int,Int], 1 -> Map.empty[Int,Int])

for(l <- src.getLines) {
  val cols = l.split("\t")
  for((cl,i) <- cols.zipWithIndex) {
    val value = cl.toInt
    val rng = (0 until ranges.size).find(r => value >= ranges(r)._1 && value <= ranges(r)._2).getOrElse(sys.error("value out of space: " + value))
    val before = results(i).getOrElse(rng, 0)
    results(i)(rng) = before + 1
  }
}

println(results)
