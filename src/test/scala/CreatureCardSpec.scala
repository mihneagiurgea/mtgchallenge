package test.scala

import org.scalatest._
import main.scala.CreatureCard

class CreatureCardSpec extends FlatSpec {

  "CreatureCard" should "return identical instances for same arguments" in {
    val c1 = CreatureCard(1, 3)
    val c2 = CreatureCard(2, 3)
    assert(!(c1 === c2))
    assert(!(c1 eq c2))
    assert(!(c1 equals c2))

    val c3 = CreatureCard(1, 3)
    assert(c1 === c3)
    assert(c1 eq c3)
    assert(c1 equals c3)
  }

  it should "have the same hashCode values for identical objects" in {
    val cr1 = CreatureCard(2, 2)
    val cr2 = CreatureCard(2, 2)

    assert(cr1.hashCode === cr2.hashCode)
  }

  it should "throw IllegalArgumentException if given invalid power or toughness" in {
    intercept[IllegalArgumentException] {
      CreatureCard(-1, 0)
    }
    intercept[IllegalArgumentException] {
      CreatureCard(1, -1)
    }
    CreatureCard(0, 0)
  }

  it should "recognize strings" in {
    val s = "2/0"
    val cr = CreatureCard.fromString(s)
    assert(cr.toString === s)
  }

  it should "be partially ordered in regards to strictly better" in {
    assert(
      CreatureCard(1, 3).tryCompareTo(CreatureCard(1, 4)) === Some(-1))
    assert(
      CreatureCard(1, 3).tryCompareTo(CreatureCard(0, 3)) === Some(+1))
    assert(
      CreatureCard(1, 3).tryCompareTo(CreatureCard(1, 3)) === Some(0))
    assert(
      CreatureCard(1, 3).tryCompareTo(CreatureCard(2, 2)) === None)
  }

}
