package test.scala

import org.scalatest.FlatSpec
import main.scala.MinimaxStrategy
import main.scala.GameState

class MinimaxStrategySpec extends FlatSpec {

  val successorCombatStepExamples = List(
   (
      "20/20 (1/CombatStep): 4/4 (TA), 3/5 (TA), 4/6 (TA) vs " +
        "4/4 (B#0), 3/3 (B#1), 2/2 (B#1), 4/4 (B#2), 3/3 (B#2)",
      Set(
        "20/20 (2/DeclareAttackers):  vs 2/2, 3/3")
    ),
    (
      "20/20 (1/CombatStep): 5/5 (TA) vs 3/3 (B#0), 2/4 (B#0), 1/1 (B#0)",
      Set(
        "20/20 (2/DeclareAttackers):  vs 3/3",
        "20/20 (2/DeclareAttackers):  vs 2/4")
    )
  )

  val successorDeclareBlockersExamples = List(
    (
      "20/20 (1/DeclareBlockers): 4/4 (TA) vs 3/3, 3/3, 1/1",
      Set(
        "20/20 (1/CombatStep): 4/4 (TA) vs 3/3, 3/3, 1/1",
        "20/20 (1/CombatStep): 4/4 (TA) vs 3/3, 3/3, 1/1 (B#0)",
        "20/20 (1/CombatStep): 4/4 (TA) vs 3/3 (B#0), 3/3 (B#0), 1/1")
    )
  )

  val NEXT_STATES_EXAMPLES = List(
      (
        "20/20 (1/DeclareAttackers): 2/3, 4/6, 1/1 (T) vs 3/1, 1/1 (T)",
        Set(
          "20/20 (2/DeclareAttackers): 2/3, 4/6, 1/1 (T) vs 3/1, 1/1",
          "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6, 1/1 (T) vs 3/1, 1/1 (T)",
          "20/20 (1/DeclareBlockers): 2/3, 4/6 (TA), 1/1 (T) vs 3/1, 1/1 (T)",
          "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1, 1/1 (T)"
        )
      ),
      (
        "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1",
        Set(
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1",
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1 (B#0)",
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1 (B#1)"
        )
      )
  )

  val strategy = MinimaxStrategy()

  "MinimaxStrategy" should "compute successor function during CombatStep" in {
    successorCombatStepExamples.foreach(assertNextStates(_))
  }

  it should "compute successor function during DeclareBlockers" in {
    successorDeclareBlockersExamples.foreach(assertNextStates(_))
  }

  def assertNextStates(example: (String, Set[String])): Unit =
    assertNextStates(example._1, example._2)

  def assertNextStates(
      gameStateString: String, expectedStatesStrings: Set[String]): Unit = {
    val gameState = GameState.fromString(gameStateString)
    val expectedStates = expectedStatesStrings.map(GameState.fromString(_))

    val nextStates = strategy.successor(gameState)
    assert(nextStates.size === expectedStates.size,
      s" incorrect number of next states for $gameState")
    assert(nextStates.toSet === expectedStates.toSet)
  }

}