package insane
package dataflow

import CFG.CFGVertex

trait LatticeAbs[E <: EnvAbs[_, S], S] {
    type Env = E
    type Vertex = CFGVertex[S]

    def join (env: E*): E

    val bottom: E
}
