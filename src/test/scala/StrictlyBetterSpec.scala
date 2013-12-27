package test.scala

// This import was added because compiler warning said so :)
import scala.language.implicitConversions

import org.scalatest.FlatSpec

import main.scala.StrictlyBetter
import main.scala.StrictlyBetter._

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

  val points = Set[Point](
    (1, 2), (2, 1), (2, 4), (3, 3), (4, 3), (5, 3), (6, 1))

  it should "compute maximals and minimals from a set of partially ordered items" in {
    assert(StrictlyBetter.maximals(points) ===
      Set[Point]((2, 4), (5, 3), (6, 1)))

    assert(StrictlyBetter.minimals(points) ===
      Set[Point]((1, 2), (2, 1)))
  }

  val s1 = Set[Point]((1, 4), (3, 2))
  val s2 = Set[Point]((0, 0), (1, 1))
  val s3 = Set[Point]((4, 5))
  val s4 = Set[Point]((2, 3))
  val s5 = Set[Point]((3, 2), (1, 4))

  it should "tryCompare Sets of PartiallyOrdered items" in {
    assert(StrictlyBetter.tryCompare(s1, s2) === Some(+1))
    assert(StrictlyBetter.tryCompare(s2, s1) === Some(-1))
    assert(StrictlyBetter.tryCompare(s1, s3) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s1) === Some(+1))
    assert(StrictlyBetter.tryCompare(s2, s3) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s2) === Some(+1))

    assert(StrictlyBetter.tryCompare(s1, s4) === None)
    assert(StrictlyBetter.tryCompare(s2, s4) === Some(-1))
    assert(StrictlyBetter.tryCompare(s3, s4) === Some(+1))

    assert(StrictlyBetter.tryCompare(s1, s5) === Some(0))

    val p1 = Point(1, 4)
    val p2 = Point(3, 2)
    val p3 = Point(2, 7)
    assert(StrictlyBetter.tryCompare(Set(p1), Set(p2)) === None)
    assert(StrictlyBetter.tryCompare(Set(p1), Set(p3)) === Some(-1))
    assert(StrictlyBetter.tryCompare(Set(p3), Set(p1)) === Some(+1))
  }

  it should "view Sets of PartiallyOrdered as PartiallyOrdered" in {
    assert(s1 > s2)
  }

  it should "compute maximals and minimals from a set of sets of " +
      "partially ordered elements" in {
    assert(
      StrictlyBetter.maximals(Set(s1, s2, s3, s4, s5)) === Set(s3))
    assert(
      StrictlyBetter.maximals(Set(s1, s2, s4, s5)) === Set(s1, s4))
  }

}
