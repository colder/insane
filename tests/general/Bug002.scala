sealed abstract class Pos {
  val x: Int
  val y: Int

  val a = new Object;
}

case class Pos1(val x: Int, val y: Int)  extends Pos {

}
