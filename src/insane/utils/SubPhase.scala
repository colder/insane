package insane
package utils

trait SubPhase {
  def run()
  val name: String

  def andThen(ph: SubPhase): SubPhases = {
    SubPhases(Seq(this)) andThen ph
  }

  import language.implicitConversions

  implicit def subPhaseToSubPhases(ph: SubPhase): SubPhases = SubPhases(Seq(ph))
}

case class SubPhases(phases: Seq[SubPhase]) {
  def andThen(ph: SubPhase): SubPhases = {
    SubPhases(phases :+ ph)
  }
}
