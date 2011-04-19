package insane
package analysis


trait Context extends Functions {
  self: AnalysisComponent =>

  import global._

  var funDecls = Map[Symbol, AbsFunction]()

  val classAnalysisGraph    = new ClassAnalysisGraph
  val classDescendentGraph  = new ClassDescendentGraph
  var purityResults         = Map[Symbol, PurityInfo]()
}
