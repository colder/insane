package insane
package analysis

import utils.SCC


trait Context extends Functions {
  self: AnalysisComponent =>

  import global._

  var funDecls = Map[Symbol, AbsFunction]()

  val callGraph             = new CallGraph
  var callGraphSCCs         = Seq[SCC[CAVertex]]()
  val classDescendentGraph  = new ClassDescendentGraph

  // Contains, for every possible Apply, the list of symbols that it potentially points to, and whether it is exhaustive
  var callTargets           = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()
  // Stores the result of point-to analysis for each method symbol
  var pointToEnvs           = Map[Symbol, PTEnv]()
  // Stores results of purity information
  var purityResults         = Map[Symbol, PurityInfo]()
}
