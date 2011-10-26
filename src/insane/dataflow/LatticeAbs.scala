package insane
package dataflow

import CFG.CFGVertex

trait LatticeAbs[E <: EnvAbs[_]] {
    type Env = E
    type Vertex = CFGVertex

    def join (env: E*): E

    val bottom: E
}
