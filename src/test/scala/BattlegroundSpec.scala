package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard
import main.scala.Battleground

class BattlegroundSpec extends FlatSpec {

  val c1 = Creature(CreatureCard(1, 1))
  val c2 = Creature(CreatureCard(2, 2))
  val c3 = Creature(CreatureCard(3, 3), tapped=true)

  val battleground =
      Battleground().addCreature(c1, 1).addCreature(c2, 1).addCreature(c3, 2)

  "Battleground" should "add creatures under some player's control" in {
    val battleground2 =
      Battleground().addCreature(c3, 2).addCreature(c2, 1).addCreature(c1, 1)

    assert(battleground === battleground2)
  }

  it should "remove creatures" in {
    assert(battleground.removeAt(0, 1) ===
      Battleground().addCreature(c2, 1).addCreature(c3, 2))

    val empty = battleground.removeAt(0, 1).removeAt(0, 1).removeAt(0, 2)
    assert(Battleground() === empty)
  }

  it should "serialize and deserialize objects" in {
    assert(battleground === Battleground.fromString(battleground.toString))

    val emptyObj = Battleground()
    assert(emptyObj === Battleground.fromString(emptyObj.toString))
  }

  it should "return a list of creatures under some player's control" in {
    assert(battleground.creatures(1) === List(c1, c2))
    assert(battleground.creatures(2) === List(c3))
  }

}
