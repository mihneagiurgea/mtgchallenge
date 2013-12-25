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

  it should "tryCompare Sets of PartiallyOrdered items" in {
    val s1 = Set[Point]((1, 4), (3, 2))
    val s2 = Set[Point]((0, 0), (1, 1))
    val s3 = Set[Point]((4, 5))

    assert(StrictlyBetter.tryCompare(s1, s2) === Some(+1))
    assert(StrictlyBetter.tryCompare(s2, s1) === Some(-1))
    assert(StrictlyBetter.tryCompare(s1, s3) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s1) === Some(+1))
    assert(StrictlyBetter.tryCompare(s2, s3) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s2) === Some(+1))

    val s4 = Set[Point]((2, 3))
    assert(StrictlyBetter.tryCompare(s1, s4) === None)
    assert(StrictlyBetter.tryCompare(s2, s4) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s4) === Some(+1))

    val s5 = Set[Point]((3, 2), (1, 4))
    println(s"s1: $s1 | s5: $s5")
    assert(StrictlyBetter.tryCompare(s1, s5) === Some(0))
  }

}
