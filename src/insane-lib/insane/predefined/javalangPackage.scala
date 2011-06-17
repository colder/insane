package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Package")
class javalangPackage {
  @AbstractsMethod("java.lang.Package.getName(()java.lang.String)")
  def getName() : java.lang.String = { "" }

}
