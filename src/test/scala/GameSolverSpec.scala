package test.scala

import org.scalatest.FlatSpec
import main.scala._
import main.scala.Outcome._

class GameSolverSpec extends FlatSpec {

  val OutcomeExamples = List(
    ("20/20 (1/DeclareAttackers): 10/10 vs ", Outcome.Win),
    ("20/20 (2/DeclareAttackers): 10/10 vs ", Outcome.Loss),
    ("11/11 (1/DeclareAttackers): 10/10 vs 9/9", Outcome.Win),
    ("20/20 (1/DeclareAttackers): 10/10 vs 3/3", Outcome.Win),
    ("20/20 (1/DeclareBlockers): 10/10 vs 3/3", Outcome.Loss),
    ("20/20 (1/CombatStep): 10/10 vs 3/3", Outcome.Win),
    ("20/20 (2/DeclareAttackers): 10/10 vs 3/3", Outcome.Loss),
    ("20/20 (1/DeclareAttackers): 7/7 vs 3/3, 3/3, 3/3", Outcome.Draw),
    ("20/20 (1/DeclareAttackers): 7/7 vs 3/3, 3/3, 3/3, 1/1", Outcome.Draw),
    ("20/20 (1/DeclareAttackers): 10/10 vs 3/3, 3/3, 3/3, 3/3, 1/1", Outcome.Loss)
  )

  case class Node(
      label: Int, nextToAct: Int, outcome: Outcome = Outcome.NotOver)
    extends GameNode {

    def isLeaf = outcome != Outcome.NotOver
  }

  "GameSolver" should "determine outcomes in a directed acyclical graph" in {
    val n1 = Node(1, 1)
    val n2 = Node(2, 2)
    val n4 = Node(3, 2, Outcome.Loss)
    val n3 = Node(4, 1, Outcome.Loss)

    val edges = Map[Node, Set[Node]](
      n1 -> Set(n2, n4),
      n2 -> Set(n3)
    )

    val expectedOutcomes = Map(
      n1 -> Outcome.Win,
      n2 -> Outcome.Win,
      n3 -> Outcome.Loss,
      n4 -> Outcome.Loss
    )
    val solver = GameSolver(edges)
    assert(solver.solveGraph(n1) === expectedOutcomes)
  }

  it should "determine outcomes in a directed cyclical graph" in {
    val n1 = Node(1, 1)
    val n2 = Node(2, 2)
    val n3 = Node(3, 2, Outcome.Loss)

    val edges = Map[Node, Set[Node]](
      n1 -> Set(n2, n3),
      n2 -> Set(n1)
    )

    val expectedOutcomes = Map(
      n1 -> Outcome.Win,
      n2 -> Outcome.Loss,
      n3 -> Outcome.Loss
    )

    val solver = GameSolver(edges)
    assert(solver.solveGraph(n1) === expectedOutcomes)
  }

  it should "determine draws in a directed cyclical graph - scenario 1" in {
    val n1 = Node(1, 1)
    val n2 = Node(2, 2)
    val n3 = Node(3, 2)

    val edges = Map[Node, Set[Node]](
      n1 -> Set(n2, n3),
      n2 -> Set(n1),
      n3 -> Set(n1)
    )

    val expectedOutcomes = Map(
      n1 -> Outcome.Draw,
      n2 -> Outcome.Draw,
      n3 -> Outcome.Draw
    )

    val solver = GameSolver(edges)
    assert(solver.solveGraph(n1) === expectedOutcomes)
  }

  it should "determine draws in a directed cyclical graph - scenario 2" in {
    val n1 = Node(1, 1)
    val n2 = Node(2, 2)
    val n3 = Node(3, 1)
    val n4 = Node(4, 2)
    val n5 = Node(5, 2, Outcome.Win)

    val edges = Map[Node, Set[Node]](
      n1 -> Set(n2, n5),
      n2 -> Set(n3),
      n3 -> Set(n4),
      n4 -> Set(n1)
    )

    val expectedOutcomes = Map(
      n1 -> Outcome.Draw,
      n2 -> Outcome.Draw,
      n3 -> Outcome.Draw,
      n4 -> Outcome.Draw,
      n5 -> Outcome.Win
    )

    val solver = GameSolver(edges)
    assert(solver.solveGraph(n1) === expectedOutcomes)
  }

  it should "determine the outcome of a given GameState" in {
    val examples =
      OutcomeExamples.map(ex => (GameState.fromString(ex._1), ex._2))
    examples.foreach(
      { case (gameState, outcome) => {
          val startTime = System.currentTimeMillis()

          val strategy = BruteForceStrategy()
          val solver = GameSolver(
            strategy.getNextStates, PredictingHeuristic.predictOutcome)
          val nodeToOutcome = solver.solveGraph(gameState)

          val duration = System.currentTimeMillis() - startTime

          assert(nodeToOutcome(gameState) === outcome, s" for $gameState")
          if (duration > 50) {
            info(f"solved $gameState")
            info(f"graph size ${nodeToOutcome.size}%,8d nodes in ${duration}%,5d ms")
          }
      } })
  }

}
