package test.scala

import org.scalatest.FlatSpec

import main.scala.StrictlyBetter

class StrictlyBetterSpec extends FlatSpec {

  "StrictlyBetter" should "combine two tryCompareTo results into one" in {
    assert(StrictlyBetter.combine(-13, -17) === Some(-1))
    assert(StrictlyBetter.combine(-13,   0) === Some(-1))
    assert(StrictlyBetter.combine(-13, +17) === None)

    assert(StrictlyBetter.combine(  0, -17) === Some(-1))
    assert(StrictlyBetter.combine(  0,   0) === Some(0))
    assert(StrictlyBetter.combine(  0, +17) === Some(+1))

    assert(StrictlyBetter.combine(+13, -17) === None)
    assert(StrictlyBetter.combine(+13,  0) === Some(+1))
    assert(StrictlyBetter.combine(+13, +17) === Some(+1))

    assert(StrictlyBetter.combine(None, Some(17)) === None)
    assert(StrictlyBetter.combine(Some(13), None) === None)
    assert(StrictlyBetter.combine(None, None) === None)
  }

}
