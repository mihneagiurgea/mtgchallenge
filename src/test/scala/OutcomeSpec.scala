package test.scala

import org.scalatest.FlatSpec
import main.scala.Outcome

class OutcomeSpec extends FlatSpec {

  "Outcome" should "reverse outcome" in {
    assert(Outcome.reverse(Outcome.Win) === Outcome.Loss)
    assert(Outcome.reverse(Outcome.Loss) === Outcome.Win)
    assert(Outcome.reverse(Outcome.Draw) === Outcome.Draw)
  }

  it should "determine the best outcome between two" in {
    assert(Outcome.max(Outcome.Loss, Outcome.Draw) === Outcome.Draw)
    assert(Outcome.max(Outcome.Draw, Outcome.Loss) === Outcome.Draw)
    assert(Outcome.max(Outcome.Draw, Outcome.Win) === Outcome.Win)
    assert(Outcome.max(Outcome.Win, Outcome.Draw) === Outcome.Win)
  }
}
