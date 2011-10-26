package insane
package dataflow

import CFG.CFGVertex

trait EnvAbs[E <: EnvAbs[_]] {
    type Env = E
    type Vertex = CFGVertex

    def duplicate: E;
}
