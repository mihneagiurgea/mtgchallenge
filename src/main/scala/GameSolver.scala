package main.scala

import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}
import Outcome._

case class GameSolver[T <: GameNode](strategy: GameGraph[T]) {

  // Current implementation uses mutable collections.
  // TODO - convert to purely functional
  val visited = MutableSet[T]()
  val nodeToOutcome = MutableMap[T, Outcome]()

  def solve(node: T): Outcome = {

    def computeOutcome(node: T, parent: T): Outcome = {
      // Determine the outcome of some node, given its DFS tree parent.
      // The output will be determined by looking at the type of the
      // (parent -> node) edge.
      val outcome = {
        if (visited contains node) {
            if (nodeToOutcome contains node)
              // ForwardEdge or CrossEdge => no cycle between these nodes.
              nodeToOutcome(node)
            else {
              // BackEdge => there is a cycle containing parent and node.
              println(s"BackEdge $node -> $parent")
              Outcome.Draw
            }
        } else
          // TreeEdge
          solve(node)
      }

      if (node.nextToAct == parent.nextToAct) outcome
      else Outcome.reverse(outcome)
    }

    visited.add(node)

    nodeToOutcome(node) = {
      if (node.isLeaf) node.outcome
      else {
        // TODO - optimize by stopping when finding an Outcome.Win
        strategy.getNextStates(node).foldLeft(Outcome.Loss)(
          (outcome, next) => {
            // println(s"\t$node -> $next")
            val o = computeOutcome(next, node)
            Outcome.max(outcome, o)
          })
      }
    }

    // println(s"Outcome[ $node ] = ${nodeToOutcome(node)}")
    nodeToOutcome(node)
  }

}
