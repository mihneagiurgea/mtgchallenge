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

  case class Point(x: Int, y: Int) extends PartiallyOrdered[Point] {

    def tryCompareTo[B >: Point](that: B)
        (implicit arg0: (B) ⇒ PartiallyOrdered[B]) = {
      val pt = that.asInstanceOf[Point]
      StrictlyBetter.combine(x - pt.x, y - pt.y)
    }

  }

  implicit def tupleToPoint(t: (Int, Int)) = Point(t._1, t._2)

  val points = List[Point](
    (1, 2), (2, 1), (2, 4), (3, 3), (4, 3), (5, 3), (6, 1))

  it should "compute maximals and minimals from a list of partially ordered items" in {
    assert(StrictlyBetter.maximals(points).toSet ===
      List[Point]((2, 4), (5, 3), (6, 1)).toSet)

    assert(StrictlyBetter.minimals(points).toSet ===
      List[Point]((1, 2), (2, 1)).toSet)
  }

}
