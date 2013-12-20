package test.scala

import org.scalatest.FlatSpec
import main.scala.BruteForceStrategy
import main.scala.GameSolver
import main.scala.GameState
import main.scala.Outcome
import main.scala.Outcome._
import main.scala.GameNode

class GameSolverSpec extends FlatSpec {

  val OutcomeExamples = List(
    // ("20/20 (1/DeclareAttackers): 10/10 vs ", Outcome.Win),
    // ("20/20 (2/DeclareAttackers): 10/10 vs ", Outcome.Loss),
    ("11/11 (1/DeclareAttackers): 10/10 vs 9/9", Outcome.Win),
    ("20/20 (1/DeclareAttackers): 10/10 vs 3/3", Outcome.Win),
    ("20/20 (1/DeclareBlockers): 10/10 vs 3/3", Outcome.Loss),
    ("20/20 (1/CombatStep): 10/10 vs 3/3", Outcome.Win),
    ("20/20 (2/DeclareAttackers): 10/10 vs 3/3", Outcome.Loss),
    ("20/20 (1/DeclareAttackers): 7/7 vs 3/3, 3/3, 3/3", Outcome.Draw),
    ("20/20 (1/DeclareAttackers): 7/7 vs 3/3, 3/3, 3/3, 1/1", Outcome.Draw),
    ("20/20 (1/DeclareAttackers): 10/10 vs 3/3, 3/3, 3/3, 5/5, 5/5", Outcome.Loss)
  )

  case class Node(
      nextToAct: Int, outcome: Outcome = Outcome.NotOver)
    extends GameNode {

    def isLeaf = outcome != Outcome.NotOver
  }

  "GameSolver" should "determine outcomes in a directed acyclical graph" in {
    val n1 = Node(1)
    val n2 = Node(2)
    val n4 = Node(2, Outcome.Loss)
    val n3 = Node(1, Outcome.Loss)

    val edges = Map[Node, Iterator[Node]](
      n1 -> Iterator(n2, n4),
      n2 -> Iterator(n3)
    )

    val solver = GameSolver(edges)
    assert(solver.solve(n1) === Outcome.Win)
    assert(solver.nodeToOutcome(n2) === Outcome.Win)
  }

  it should "determine outcomes in a directed cyclical graph" in {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(2, Outcome.Loss)

    val edges = Map[Node, Iterator[Node]](
      n1 -> Iterator(n2, n3),
      n2 -> Iterator(n1)
    )

    val solver = GameSolver(edges)
    assert(solver.solve(n1) === Outcome.Win)
    assert(solver.nodeToOutcome(n2) === Outcome.Loss)
  }

  it should "determine draws in a directed cyclical graph - scenario 1" in {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(2)

    val edges = Map[Node, Iterator[Node]](
      n1 -> Iterator(n2, n3),
      n2 -> Iterator(n1),
      n3 -> Iterator(n1)
    )

    val solver = GameSolver(edges)
    assert(solver.solve(n1) === Outcome.Draw)
    assert(solver.nodeToOutcome(n2) === Outcome.Draw)
    assert(solver.nodeToOutcome(n3) === Outcome.Draw)
  }

  it should "determine draws in a directed cyclical graph - scenario 2" in {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(1)
    val n4 = Node(2)
    val n5 = Node(2, Outcome.Win)

    val edges = Map[Node, Iterator[Node]](
      n1 -> Iterator(n2, n5),
      n2 -> Iterator(n3),
      n3 -> Iterator(n4),
      n4 -> Iterator(n1)
    )

    val solver = GameSolver(edges)
    assert(solver.solve(n1) === Outcome.Draw)
    assert(solver.nodeToOutcome(n2) === Outcome.Draw)
    assert(solver.nodeToOutcome(n3) === Outcome.Draw)
    assert(solver.nodeToOutcome(n4) === Outcome.Draw)
  }

  it should "determine the outcome of a given GameState" in {
    val examples =
      OutcomeExamples.map(ex => (GameState.fromString(ex._1), ex._2))
    examples.foreach(
      { case (gameState, outcome) => {
          val strategy = BruteForceStrategy()
          val solver = GameSolver(strategy.getNextStates)
          assert(solver.solve(gameState) === outcome, s" for $gameState")
      } })
  }

}
