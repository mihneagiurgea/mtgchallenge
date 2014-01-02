package test.scala

import org.scalatest.FlatSpec
import main.scala.EvaluationFunction
import main.scala.GameState

class EvaluationFunctionSpec extends FlatSpec {

  val f = EvaluationFunction

  "EvaluationFunction" should "evaluate more life as better" in {
    val gs1 = GameState.fromString("20/20 (1/DeclareAttackers):  vs ")
    val gs2 = GameState.fromString("30/20 (1/DeclareAttackers):  vs ")

    assert(f(gs1) < f(gs2))
  }

  it should "evaluate bigger or more creatures as better" in {
    val gs1 = GameState.fromString("20/20 (2/DeclareAttackers): vs 3/3, 3/3")
    val gs2 = GameState.fromString("20/20 (2/DeclareAttackers): vs 3/3")
    val gs3 = GameState.fromString("20/20 (2/DeclareAttackers): vs 2/2, 1/1")

    assert(f(gs1) > f(gs2))
    assert(f(gs1) > f(gs3))
  }

  it should "evaluate simmetric states with opposite values" in {
    val gs1 = GameState.fromString("20/20 (1/DeclareAttackers): 1/5 vs 3/3")
    val gs2 = gs1.endCurrentTurn()

    assert(f(gs1) === -f(gs2))
  }

}
