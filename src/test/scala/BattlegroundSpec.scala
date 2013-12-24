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

  "Battleground" should "add creatures under some player's control and " +
      "maintain a sorted order" in {
    assert(battleground ===
      Battleground().addCreature(c3, 2).addCreature(c2, 1).addCreature(c1, 1))
    assert(battleground ===
      Battleground().addCreature(c2, 1).addCreature(c1, 1).addCreature(c3, 2))
    assert(battleground ===
      Battleground().addCreature(c1, 1).addCreature(c3, 2).addCreature(c2, 1))
  }

  it should "serialize and deserialize objects" in {
    assert(battleground === Battleground.fromString(battleground.toString))

    val emptyObj = Battleground()
    assert(emptyObj === Battleground.fromString(emptyObj.toString))
  }

  it should "return a list of creatures under some player's control" in {
    assert(battleground(1) === List(c1, c2))
    assert(battleground(2) === List(c3))
  }

  it should "remove many creatures at a time" in {
    assert(battleground.removeMany(Set(1, 0), Set(0)) === Battleground())

    assert(battleground.removeMany(Set(1), Set(0)) ===
      Battleground().addCreature(c1, 1))
  }

  it should "filter creatures with Index by controller and some predicate" in {
    val pred = (creature: Creature) => creature.power >= 2
    assert(battleground.filterWithIndex(1, pred) ===  List((c2, 1)))
  }

}
