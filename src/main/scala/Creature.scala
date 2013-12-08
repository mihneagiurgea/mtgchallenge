package main.scala

import scala.util.matching.Regex

/** Factory for Creature instances. */
object Creature {

  // Bit-related constants
  val BlockingBitOffset = 2
  val AttackingBitMask = (1 << 1)
  val TappedBitMask = 1

  def apply(creatureCard: CreatureCard, tapped: Boolean = false,
            attacking: Boolean = false, blockedId: Int = 0): Creature = {
    require(blockedId >= 0)
    require(!(blockedId > 0 && attacking))

    var state = blockedId << BlockingBitOffset
    if (attacking) state |= AttackingBitMask
    if (tapped) state |= TappedBitMask
    new Creature(creatureCard, state)
  }

  def fromString(s: String) = {
    val split = s.split(' ')
    val creatureCard = CreatureCard.fromString(split(0))

    val stateString = if (split.length == 2) split(1) else ""

    val tapped = stateString.contains('T')
    val attacking = stateString.contains('A')

    val regex = new Regex("B#(\\d+)", "blockedIdString")
    val blockedId: Int = regex findFirstIn stateString match {
      case Some(regex(blockedIdString)) => blockedIdString.toInt
      case None => 0
    }

    Creature(creatureCard, tapped=tapped, attacking=attacking, blockedId=blockedId)
  }
}

/** A stateful Creature.
  *
  * Should be constructed using the companion object.
  */
case class Creature private(creatureCard: CreatureCard, state: Int) {
  import Creature._

  require(creatureCard != null)
  require(state >= 0)

  // Getters for CreatureCard attributes. This implementation might
  // change as CreatureCards get more attributes (trample, flying, etc.)
  def power: Int = creatureCard.power
  def toughness: Int = creatureCard.toughness

  def isTapped: Boolean = (state & TappedBitMask) == 1
  def isAttacking: Boolean = (state & AttackingBitMask) == AttackingBitMask
  def isBlocking: Boolean = (state >> BlockingBitOffset) != 0
  def blockedId: Int = state >> BlockingBitOffset

  override def toString = {
    val state = (if (isTapped) "T" else "") +
                (if (isAttacking) "A" else "") +
                (if (isBlocking) s"B#$blockedId" else "")
    state match {
      case "" => s"$creatureCard"
      case _ => s"$creatureCard ($state)"
    }
  }

}
