package insane
package analysis

import CFG.VertexImp

abstract class DataFlowEnvAbs[E <: DataFlowEnvAbs[_, S], S] {
    type Env = E
    type Vertex = VertexImp[S]

    def union (env: E): E;
    def copy: E;
}
