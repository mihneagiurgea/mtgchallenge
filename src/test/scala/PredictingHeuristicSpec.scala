package test.scala

import org.scalatest.FlatSpec
import main.scala.GameState
import main.scala.Outcome
import main.scala.Outcome._
import main.scala.PredictingHeuristic.predictOutcome

class PredictingHeuristicSpec extends FlatSpec {

  "PredictingHeuristic" should "predict leaf GameState" in {
    assert(predictOutcome(GameState(-1, 20)) === Outcome.Loss)
    assert(predictOutcome(GameState(20, -1)) === Outcome.Win)
  }

  it should "predict draws when no creatures on the battlefield" in {
    assert(predictOutcome(GameState()) === Outcome.Draw)
    assert(predictOutcome(GameState(20, 1)) === Outcome.Draw)
  }

  it should "predict a win when only one player has creatures and they can attack" in {
    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareAttackers): 10/10 vs "))
      === Outcome.Win)

    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareBlockers): 10/10 (TA) vs "))
      === Outcome.Loss)

    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareAttackers): 1/1, 2/2 vs "))
      === Outcome.Win)
  }

  it should "predict a draw when only one player has creatures and they have 0 power" in {
    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareAttackers): 0/4, 0/7 vs "))
      === Outcome.Draw)
  }

  it should "predict a draw when both players have creatures but none can attack" in {
    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareAttackers): 0/7 vs 0/4, 0/5"))
      === Outcome.Draw)
  }

  it should "not make any prediction when both players have creatures" in {
    assert(
      predictOutcome(
        GameState.fromString("20/20 (1/DeclareAttackers): 3/3 vs 1/1, 2/2"))
      === Outcome.NotOver)
  }

}
