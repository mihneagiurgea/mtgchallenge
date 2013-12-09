package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard
import main.scala.PlayerBattleground

class PlayerBattlegroundSpec extends FlatSpec {

  val c1 = Creature(CreatureCard(1, 1))
  val c2 = Creature(CreatureCard(2, 2))
  val c3 = Creature(CreatureCard(3, 3), tapped=true)

  "PlayerBattleground" should "add creatures in sorted order" in {
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground(c3, c2, c1))
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground() + c1 + c2 + c3)
    assert(PlayerBattleground(c1, c2, c3) === PlayerBattleground(c1) + c2 + c3)

    assert(PlayerBattleground(c1, c2, c3) !== PlayerBattleground(c1, c2))
  }

  it should "remove creatures" in {
    assert(PlayerBattleground(c1, c2, c3).removeAt(0) === PlayerBattleground(c3, c2))
  }

  it should "serialize and deserialize objects" in {
    val obj = PlayerBattleground(c1, c2, c3)
    assert(obj === PlayerBattleground.fromString(obj.toString))

    val emptyObj = PlayerBattleground()
    assert(emptyObj === PlayerBattleground.fromString(emptyObj.toString))
    assert(emptyObj === PlayerBattleground.fromString(" "))
  }
}
