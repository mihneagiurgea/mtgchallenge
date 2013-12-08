package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard

class CreatureSpec extends FlatSpec {

  "Creature" should "return equivalent instances for same arguments only" in {
    val cc = CreatureCard(1, 1)
    val c1 = Creature(cc, tapped=true)
    val c2 = Creature(cc, tapped=true, attacking=true)

    assert(c1 !== c2)
    assert(!(c1 eq c2))
    assert(!(c1 equals c2))
    assert(c1.hashCode !== c2.hashCode)

    val c3 = Creature(cc, tapped=true)
    assert(c1 === c3)
    assert(!(c1 eq c3))
    assert(c1 equals c3)
    assert(c1.hashCode === c3.hashCode)
  }

  it should "define accessors for all its properties" in {
    val cc = CreatureCard(1, 1)
    var creature = Creature(cc)
    assert(!creature.isTapped)
    assert(!creature.isAttacking)
    assert(!creature.isBlocking)
    assert(creature.blockedId == 0)

    creature = Creature(cc, tapped=true, attacking=true)
    assert(creature.isTapped)
    assert(creature.isAttacking)
    assert(!creature.isBlocking)
    assert(creature.blockedId == 0)

    creature = Creature(cc, blockedId=3)
    assert(!creature.isTapped)
    assert(!creature.isAttacking)
    assert(creature.isBlocking)
    assert(creature.blockedId === 3)
  }

  it should "define getters for power and toughness" in {
    val cc = CreatureCard(1, 2)
    val creature = Creature(cc)

    assert(cc.power == 1)
    assert(cc.toughness == 2)
  }

  it should "recognize objects from strings" in {
    var creature = Creature.fromString("2/3 (TA)")
    assert(creature.power == 2)
    assert(creature.toughness == 3)

    assert(creature.isTapped)
    assert(creature.isAttacking)
    assert(!creature.isBlocking)

    creature = Creature.fromString("2/3 (B#47)")
    assert(!creature.isTapped)
    assert(!creature.isAttacking)
    assert(creature.isBlocking)
    assert(creature.blockedId == 47)

    creature = Creature.fromString("2/3")
    assert(!creature.isTapped)
    assert(!creature.isAttacking)
    assert(!creature.isBlocking)
  }

  it should "serialize and deserialize objects" in {
    val creatureCardStrings = List("1/1 (TA)", "1/1 (A)", "1/1 (B#5)", "2/3 (T)", "0/7")
    for (creatureCardString <- creatureCardStrings)
      assert(Creature.fromString(creatureCardString).toString == creatureCardString)
  }

}