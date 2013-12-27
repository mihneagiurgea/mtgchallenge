package test.scala

import org.scalatest._
import main.scala.BruteForceStrategy
import main.scala.GameState

class BruteForceStrategySpec extends FlatSpec {

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
        "20/20 (1/DeclareBlockers): 1/1 (T), 2/3 (TA), 4/6 (TA) vs 3/1",
        Set(
          "20/20 (1/CombatStep): 1/1 (T), 2/3 (TA), 4/6 (TA) vs 3/1",
          "20/20 (1/CombatStep): 1/1 (T), 2/3 (TA), 4/6 (TA) vs 3/1 (B#1)",
          "20/20 (1/CombatStep): 1/1 (T), 2/3 (TA), 4/6 (TA) vs 3/1 (B#2)"
        )
      ),
      (
        "20/20 (1/CombatStep): 3/5 (TA), 4/4 (TA), 4/6 (TA) vs " +
        "4/4 (B#1), 3/3 (B#0), 2/2 (B#0), 4/4 (B#2), 3/3 (B#2)",
        Set(
          "20/20 (2/DeclareAttackers):  vs 3/3, 4/4",
          "20/20 (2/DeclareAttackers):  vs 2/2, 4/4",
          "20/20 (2/DeclareAttackers):  vs 3/3, 3/3",
          "20/20 (2/DeclareAttackers):  vs 2/2, 3/3"
        )
      )
  )

  val strategy = BruteForceStrategy()

  "BruteForceStrategy" should "determine next states during DeclareAttackers" in {
    assertNextStates(NEXT_STATES_EXAMPLES(0))
  }

  it should "determine next states during DeclareBlockers" in {
    assertNextStates(NEXT_STATES_EXAMPLES(1))
  }

  it should "determine next states during CombatStep" in {
    assertNextStates(NEXT_STATES_EXAMPLES(2))
  }

  def assertNextStates(example: (String, Set[String])): Unit =
    assertNextStates(example._1, example._2)

  def assertNextStates(
      gameStateString: String, expectedStatesStrings: Set[String]): Unit = {
    val gameState = GameState.fromString(gameStateString)
    val expectedStates = expectedStatesStrings.map(GameState.fromString(_))

    val nextStates = strategy.getNextStates(gameState)
    assert(nextStates.toSet === expectedStates.toSet)
  }

}
