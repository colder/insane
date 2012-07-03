package insane
package alias

import utils._
import utils.Reporters._
import GlobalCounters.withDebugCounter
import CFG._

trait PointToLattices extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  object PointToLattice extends dataflow.LatticeAbs[PTEnv] {
    val bottom = BottomPTEnv

    def join(v: CFGVertex, _envs: PTEnv*): PTEnv = {
      joinUID(new UniqueID(v.id), _envs: _*)
    }

    def joinUID(uid: UniqueID, _envs: PTEnv*): PTEnv = {
      var envs = _envs.filterNot(_.category.isBottom)

      if (envs.isEmpty) {
        return BottomPTEnv
      } else if(envs.tail.isEmpty) {
        return envs.head
      } else if (envs.exists(_.category.isTop)) {
        return TopPTEnv 
      }

      //reporter.debug("Unioning: "+uid)

      //withDebugCounter { cnt =>
      //  for ((e,i) <- envs.zipWithIndex) {
      //    dumpPTE(e, "union-"+cnt+"-"+i+".dot")
      //  }
      //}

      // println("Size: "+envs.size)

      var newNodes  = envs.flatMap(_.ptGraph.V).toSet

      /**
       * If a non-singleton INode is present in one env and a singleton node is
       * present in another, we need to replace the singleton node with the non
       * singleton one
       */
      for ((n, sNode) <- newNodes.collect{ case i @ INode(pPoint, false, types) => (i, INode(pPoint, true, types)) } if newNodes contains sNode) {
        envs = for (e <- envs) yield {
          if (e.ptGraph.V contains sNode) {
            e.replaceNode(sNode, Set(n))
          } else {
            e
          }
        }
      }

      newNodes  = envs.flatMap(_.ptGraph.V).toSet
      var newIEdges = envs.flatMap(_.iEdges).toSet
      var newOEdges = envs.flatMap(_.oEdges).toSet

      /**
       * When merging environment, we need to take special care in case one
       * write edge is not present in the other envs, in that case, it
       * consists of a weak update in the resulting env.
       */

      // 1) We find all nodes that are shared between all envs
      val commonNodes = envs.map(_.ptGraph.V).reduceRight(_ & _)

      // 2) We find all the pair (v1, f) that are not in every env's iEdges
      val allPairs = newIEdges.map(ed => (ed.v1, ed.label)).toSet

      val commonPairs = envs.map(_.iEdges.map(ed => (ed.v1, ed.label)).toSet).reduceRight(_ & _)

      for ((v1, field) <- allPairs -- commonPairs if (commonNodes contains v1) || !v1.isResolved) {
        safeLNode(v1, field, uid) match {
          case Some(lNode) =>
            //for (nodeToAdd <- findSimilarLNodes(lNode, newNodes)) {
            //  newNodes  += nodeToAdd
            //  newIEdges += IEdge(v1, field, nodeToAdd)
            //  newOEdges += OEdge(v1, field, nodeToAdd)
            //}

            newNodes  += lNode
            newIEdges += IEdge(v1, field, lNode)
            newOEdges += OEdge(v1, field, lNode)
          case None if v1 == NNode  || v1.isInstanceOf[OBNode] =>
            // ignore if this is obviously an imprecision
          case None =>
            for (e <- envs) {
              withDebugCounter { cnt => 
                dumpPTE(e, "union-"+cnt+".dot")
              }
            }
            dumpAnalysisStack()
            reporter.fatal("Unable to create LNode from "+v1+"["+v1.types+"] via "+field+" upon union!")
        }
      }

      var newGraph = new PointToGraph(newNodes, newOEdges ++ newIEdges)
      
      val allRefs    = envs.flatMap(_.locState.keySet).toSet
      //val commonRefs = envs.map(_.locState.keySet).reduceRight(_ & _)


      var env = PTEnv(
        newGraph,
        allRefs.map((k: CFG.Ref) => k -> (envs.map(e => e.locState(k)).reduceRight(_ ++ _))).toMap.withDefaultValue(Set()),
        newIEdges,
        newOEdges,
        envs.map(_.danglingCalls).reduceRight(_ ++ _),
        envs.map(_.category).reduceRight(_ lub _))


      //for (r <- allRefs -- commonRefs) {
      //  // Those are refs missing an valuation in at least one incomming env,
      //  // this thus is a weak ref updates
      //  reporter.error("PAAP: Ref "+r+" was not found in all envs!");
      //}

      //withDebugCounter { cnt =>
      //  dumpPTE(env, "union-"+cnt+"-res.dot")
      //}

      //env = env.mergeSimilarNodes

      //withDebugCounter { cnt =>
      //  dumpPTE(env, "union-"+cnt+"-merg.dot")
      //}

      env
    }

  }
}
