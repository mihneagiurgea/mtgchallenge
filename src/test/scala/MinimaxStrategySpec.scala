package test.scala

import org.scalatest.FlatSpec
import main.scala.MinimaxStrategy
import main.scala.GameState

class MinimaxStrategySpec extends FlatSpec {

  val successorCombatStepExamples = List(
   (
      "20/20 (1/CombatStep): 3/5 (TA), 4/4 (TA), 4/6 (TA) vs " +
        "4/4 (B#1), 3/3 (B#0), 2/2 (B#0), 4/4 (B#2), 3/3 (B#2)",
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
    ),
    (
      "20/20 (1/DeclareBlockers): 1/1 (TA), 4/4 (TA) vs 5/5",
      Set(
        "20/20 (1/CombatStep): 1/1 (TA), 4/4 (TA) vs 5/5 (B#1)")
    ),
    (
      "20/20 (1/DeclareBlockers): 1/1, 2/2, 3/3 (TA) vs 7/7",
      Set(
        "20/20 (1/CombatStep): 1/1, 2/2, 3/3 (TA) vs 7/7 (B#2)")
    )
  )

  val successorDeclareAttackersExamples = List(
    (
      "20/20 (1/DeclareAttackers): 3/3, 2/2, 1/1 vs 7/7",
      Set(
        "20/20 (2/DeclareAttackers): 3/3, 2/2, 1/1 vs 7/7",
        "20/20 (1/DeclareBlockers): 3/3 (TA), 2/2 (TA), 1/1 vs 7/7",
        "20/20 (1/DeclareBlockers): 3/3 (TA), 2/2, 1/1 (TA) vs 7/7",
        "20/20 (1/DeclareBlockers): 3/3, 2/2 (TA), 1/1 (TA) vs 7/7",
        "20/20 (1/DeclareBlockers): 3/3 (TA), 2/2 (TA), 1/1 (TA) vs 7/7")
    )
  )

  val strategy = MinimaxStrategy()

  "MinimaxStrategy" should "compute successor function during CombatStep" in {
    successorCombatStepExamples.foreach(assertNextStates(_))
  }

  it should "compute successor function during DeclareBlockers" in {
    successorDeclareBlockersExamples.foreach(assertNextStates(_))
  }

  it should "compute successor function during DeclareAttackers" in {
    successorDeclareAttackersExamples.foreach(assertNextStates(_))
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