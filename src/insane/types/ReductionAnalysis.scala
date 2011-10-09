package insane
package types

import utils._
import utils.Graphs._
import utils.Reporters._
import dataflow._

import CFG.CFGVertex

class ReductionAnalysis[E <: EnvAbs[E, S], S] (lattice : LatticeAbs[E, S], baseEnv : E, settings: Settings) extends Analysis[E, S](lattice, baseEnv, settings) {

}
