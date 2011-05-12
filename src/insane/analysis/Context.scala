package insane
package analysis


trait Context extends Functions {
  self: AnalysisComponent =>

  import global._

  var funDecls = Map[Symbol, AbsFunction]()

  val classAnalysisGraph    = new ClassAnalysisGraph
  val classDescendentGraph  = new ClassDescendentGraph
  // Contains, for every possible Apply, the list of symbols that it potentially points to
  var callResolutions       = Map[CFG.Tree, Set[Symbol]]()
  var purityResults         = Map[Symbol, PurityInfo]()
}
