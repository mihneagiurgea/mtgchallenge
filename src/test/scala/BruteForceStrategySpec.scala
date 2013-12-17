package test.scala

import org.scalatest._
import main.scala.BruteForceStrategy
import main.scala.GameState

class BruteForceStrategySpec extends FlatSpec {

  val NEXT_STATES_EXAMPLES = List(
      (
        "20/20 (1/DeclareAttackers): 2/3, 4/6, 1/1 (T) vs 3/1",
        Seq(
          "20/20 (2/DeclareAttackers): 2/3, 4/6, 1/1 (T) vs 3/1",
          "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6, 1/1 (T) vs 3/1",
          "20/20 (1/DeclareBlockers): 2/3, 4/6 (TA), 1/1 (T) vs 3/1",
          "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1"
        )
      ),
      (
        "20/20 (1/DeclareBlockers): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1",
        Seq(
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1",
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1 (B#0)",
          "20/20 (1/CombatStep): 2/3 (TA), 4/6 (TA), 1/1 (T) vs 3/1 (B#1)"
        )
      ),
      (
        "20/20 (1/CombatStep): 4/4 (TA), 3/5 (TA), 4/6 (TA) vs " +
        "4/4 (B#0), 3/3 (B#1), 2/2 (B#1), 4/4 (B#2), 3/3 (B#2)",
        Seq(
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
    NEXT_STATES_EXAMPLES.slice(0, 1).foreach(assertNextStates(_))
  }

  it should "determine next states during DeclareBlockers" in {
    assertNextStates(NEXT_STATES_EXAMPLES(1))
  }

  it should "determine next states during CombatStep" in {
    assertNextStates(NEXT_STATES_EXAMPLES(1))
  }

  def assertNextStates(example: (String, Seq[String])): Unit =
    assertNextStates(example._1, example._2)

  def assertNextStates(
      gameStateString: String, expectedStatesStrings: Seq[String]): Unit = {
    val gameState = GameState.fromString(gameStateString)
    val expectedStates = expectedStatesStrings.map(GameState.fromString(_))

    val nextStates = strategy.getNextStates(gameState)
    assert(nextStates.toSet === expectedStates.toSet)
  }

}
