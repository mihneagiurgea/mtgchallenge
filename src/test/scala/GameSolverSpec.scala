package test.scala

import org.scalatest.FlatSpec
import main.scala.BruteForceStrategy
import main.scala.GameSolver
import main.scala.GameState
import main.scala.Outcome

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

  "GameSolver" should "determine the outcome of a given GameState" in {
    val examples =
      OutcomeExamples.map(ex => (GameState.fromString(ex._1), ex._2))
    examples.foreach(
      { case (gameState, outcome) => {
          val solver = GameSolver(BruteForceStrategy())
          assert(solver.solve(gameState) === outcome, s" for $gameState")
      } })
  }

}
