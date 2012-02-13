package insane.annotations

import scala.annotation.Annotation

abstract class Effect extends Annotation

class Pure extends Effect
class Mod(locs: Any*) extends Effect
class Call(locs: Any*) extends Effect
