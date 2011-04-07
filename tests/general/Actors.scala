import actors.Actor

object TestActor extends Actor {
  def act() = {
    this ! Plop
  }
}

case object Plop
