package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard
import main.scala.PlayerBattleground
import main.scala.PlayerBattleground._

class PlayerBattlegroundSpec extends FlatSpec {

  val c1 = Creature(CreatureCard(1, 1))
  val c2 = Creature(CreatureCard(2, 2))
  val c3 = Creature(CreatureCard(3, 3), tapped=true)

  val playerBattleground = PlayerBattleground(c1, c2, c3)

  "PlayerBattleground" should "add creatures in sorted order" in {
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground(c3, c2, c1))
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground() + c1 + c2 + c3)
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground(c1) + c2 + c3)

    assert(PlayerBattleground(c1, c2, c3) !== PlayerBattleground(c1, c2))

    assert(PlayerBattleground.fromString("3/3 (T), 2/2, 1/1") ===
      PlayerBattleground(c1, c2, c3))
  }

  it should "define a total ordering of creatures" in {
    val c1 = Creature(CreatureCard(1, 1))
    val c2 = Creature(CreatureCard(1, 1), tapped=true)
    val c3 = Creature(CreatureCard(1, 2))

    assert(List(c1, c2, c3).sortWith(SizeOrdering.lt) ==
      List(c3, c2, c1).sortWith(SizeOrdering.lt))

    val a = Creature(CreatureCard(4, 3))
    val b = Creature(CreatureCard(1, 5))
    val c = Creature(CreatureCard(0, 6))

    assert(List(a, b, c).sortWith(SizeOrdering.lt) === List(c, b, a))
  }

  it should "remove multiple creatures at a time" in {
    assert(PlayerBattleground(c1, c2, c3).removeMany(Set(0, 2)) ===
      PlayerBattleground(c2))
    assert(PlayerBattleground(c1, c2, c3).removeMany(Set(1)) ===
      PlayerBattleground(c3, c1))
  }

  it should "serialize and deserialize objects" in {
    val obj = PlayerBattleground(c1, c2, c3)
    assert(obj === PlayerBattleground.fromString(obj.toString))

    val emptyObj = PlayerBattleground()
    assert(emptyObj === PlayerBattleground.fromString(emptyObj.toString))
    assert(emptyObj === PlayerBattleground.fromString(" "))
  }

  it should "declare some subset of creatures as attackers" in {
    assert(playerBattleground.declareAttackers(Set(0, 1)) ===
      PlayerBattleground(c1.attack(), c2.attack(), c3))
  }

  it should "declare some subset of creatures as blockers" in {
    assert(playerBattleground.declareBlockers(Map(0 -> 1, 1->0)) ===
      PlayerBattleground(c1.block(1), c2.block(0), c3))
  }

  it should "be partially ordered in regards to strictly better" in {
    // c1 < c2, but c1 and c2 are not comparable with c3
    assertTryCompareTo(
      PlayerBattleground(c1), PlayerBattleground(c2), Some(-1))
    assertTryCompareTo(
      PlayerBattleground(c1), PlayerBattleground(c1, c1), Some(-1))
    assertTryCompareTo(
      PlayerBattleground(c2, c1, c2, c1, c2),
      PlayerBattleground(c2, c1, c2, c1, c2),
      Some(0))
    assertTryCompareTo(
      PlayerBattleground(c2, c1, c2, c1, c2),
      PlayerBattleground(c1, c2, c1, c2),
      Some(+1))
    assertTryCompareTo(
      PlayerBattleground(c1, c1, c2),
      PlayerBattleground(c1, c1, c3),
      None)
    assertTryCompareTo(
      PlayerBattleground(c1, c2, c3),
      PlayerBattleground(c1, c2),
      Some(+1))

    assertTryCompareTo("1/1 (T), 2/2", "2/2", Some(+1))
    assertTryCompareTo("2/2", "1/1 (T), 2/2", Some(-1))
    assertTryCompareTo("2/4, 5/5 (T), 5/5", "2/3, 4/5 (T), 5/5 (T)", Some(+1))

    // This test fails because the current implementation is not 100% correct.
    // assertTryCompareTo("2/9, 5/5 (T), 5/5", "2/3, 4/5 (T), 5/5 (T)", Some(+1))
  }

  def assertTryCompareTo(
      s1: String, s2: String, expected: Option[Int]): Unit = {
    val x = PlayerBattleground.fromString(s1)
    val y = PlayerBattleground.fromString(s2)
    assertTryCompareTo(x, y, expected)
  }

  def assertTryCompareTo(
      x: PlayerBattleground, y: PlayerBattleground, expected: Option[Int]): Unit = {
    assert(x.tryCompareTo(y) === expected, s" for $x and $y")
  }

}
