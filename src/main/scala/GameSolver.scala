package main.scala

import scala.collection.mutable.{Queue => MutableQueue,
  Set => MutableSet, Map => MutableMap, MutableList}
import Outcome._

object GameSolver {

  def defaultPredictOutcome[T <: GameNode](node: T): Outcome =
    if (node.isLeaf) node.outcome
    else Outcome.NotOver
}

case class GameSolver[T <: GameNode](
    outgoingEdges: (T) => Iterator[T],
    predictOutcome: (T) => Outcome = GameSolver.defaultPredictOutcome[T] _) {

  type Graph = MutableMap[T, List[T]]

  val queue = MutableQueue[T]()
  val incomingEdges = MutableMap[T, MutableList[T]]().withDefault(_ => MutableList())
  val outgoingDegree = MutableMap[T, Int]().withDefaultValue(0)
  val nodeToOutcome = MutableMap[T, Outcome]()
  val visited = MutableSet[T]()

  private def dfs(node: T): Unit = {
    visited.add(node)
    val outcome = predictOutcome(node)
    if (outcome != Outcome.NotOver) {
      queue.enqueue(node)
      nodeToOutcome(node) = outcome
      return
    }
    for (next <- outgoingEdges(node)) {
      // Add (node -> next) as an incomingEdge.
      if (incomingEdges contains next)
        incomingEdges(next) += node
      else
        incomingEdges(next) = MutableList(node)

      // Increment outgoingDegree(node).
      outgoingDegree(node) += 1

      if (!(visited contains next))
        dfs(next)
    }
  }

  def solveGraph(root: T): Map[T, Outcome] = {
    dfs(root)

    // Reset visited to use as an indicator of which elements have been
    // added to queue.
    visited.clear()
    queue.map(visited.add(_))

    while (queue.length > 0) {
      val node = queue.dequeue()

      for (adj <- incomingEdges(node)) {
        // Processing edge (adj -> node)
        val outcome =
          if (adj.nextToAct == node.nextToAct) nodeToOutcome(node)
          else Outcome.reverse(nodeToOutcome(node))

        outgoingDegree(adj) -= 1
        // nodeToOutcome(adj) max= outcome
        if (outcome == Outcome.Win) {
          nodeToOutcome(adj) = Outcome.Win
        } else {
          // If all outgoing edges from adj have been exhausted and outcome
          // of adj has not yet been decided as a Win, then it will be a Loss.
          if (outgoingDegree(adj) == 0 && !nodeToOutcome.contains(adj))
            nodeToOutcome(adj) = Outcome.Loss
        }
        if (nodeToOutcome.contains(adj) && !visited.contains(adj)) {
          queue.enqueue(adj)
          visited.add(adj)
        }
      }

    }

    // All other nodes whose outcome has not been decided are a Draw.
    outgoingDegree.keys.filterNot(nodeToOutcome.contains(_)).map(
      nodeToOutcome(_) = Outcome.Draw)

    nodeToOutcome.toMap
  }

  def solve(root: T): Outcome =
    solveGraph(root)(root)

}
