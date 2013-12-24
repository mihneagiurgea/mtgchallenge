package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard
import main.scala.PlayerBattleground

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
    assert(PlayerBattleground(c1).
      tryCompareTo(PlayerBattleground(c2)) === Some(-1))
    assert(PlayerBattleground(c1).
      tryCompareTo(PlayerBattleground(c1, c1)) === Some(-1))
    assert(PlayerBattleground(c2, c1, c2, c1, c2).
      tryCompareTo(PlayerBattleground(c2, c1, c2, c1, c2)) === Some(0))
    assert(PlayerBattleground(c2, c1, c2, c1, c2).
      tryCompareTo(PlayerBattleground(c1, c2, c1, c2)) === Some(+1))

    assert(PlayerBattleground(c1, c1, c2).
      tryCompareTo(PlayerBattleground(c1, c1, c3)) === None)
  }

}
