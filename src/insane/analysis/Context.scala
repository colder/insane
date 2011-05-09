package insane
package analysis


trait Context extends Functions {
  self: AnalysisComponent =>

  import global._

  var funDecls = Map[Symbol, AbsFunction]()

  val classAnalysisGraph    = new ClassAnalysisGraph
  val classDescendentGraph  = new ClassDescendentGraph
  // Contains, for every possible Apply, the list of symbols that it potentially points to, and whether it is exhaustive
  var callTargets           = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()
  var purityResults         = Map[Symbol, PurityInfo]()
}
