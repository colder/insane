package insane
package dataflow

import CFG.CFGVertex

trait EnvAbs[E <: EnvAbs[_, S], S] {
    type Env = E
    type Vertex = CFGVertex[S]

    def duplicate: E;
}
