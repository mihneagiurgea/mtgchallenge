package main.scala

import scala.util.matching.Regex

/** Factory for Creature instances. */
object Creature {

  // Bit-related constants
  val BlockingBitOffset = 2
  val AttackingBitMask = (1 << 1)
  val TappedBitMask = 1

  def apply(creatureCard: CreatureCard, tapped: Boolean = false,
            attacking: Boolean = false, blockedId: Int = -1): Creature = {
    require(blockedId >= -1)
    require(!(blockedId >= 0 && attacking))

    var state = (blockedId + 1) << BlockingBitOffset
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
      case None => -1
    }

    Creature(creatureCard, tapped=tapped, attacking=attacking, blockedId=blockedId)
  }
}

/** A stateful Creature.
  *
  * Should be constructed using the companion object.
  * Defines an arbitrary total ordering and a "strictly better than" partial
  * ordering.
  */
case class Creature private(creatureCard: CreatureCard, state: Int)
  extends PartiallyOrdered[Creature] {

  import Creature._

  require(creatureCard != null)
  require(state >= 0)

  def tryCompareTo[B >: Creature](that: B)
      (implicit arg0: (B) ⇒ PartiallyOrdered[B]) =
    tryCompareTo(that.asInstanceOf[Creature])

  def tryCompareTo(that: Creature): Option[Int] = {
    val cmpState: Option[Int] = {
      if (this.isAttacking != that.isAttacking) None
      else if (this.isBlocking != that.isBlocking) None
      else
        if (this.isTapped == that.isTapped) Option(0)
        else
          // If this is tapped and that is not, than this < that.
          if (this.isTapped) Option(-1)
          else Option(+1)
    }

    StrictlyBetter.combine(cmpState,
      creatureCard.tryCompareTo(that.creatureCard))
  }

  // Getters for CreatureCard attributes. This implementation might
  // change as CreatureCards get more attributes (trample, flying, etc.)
  def power: Int = creatureCard.power
  def toughness: Int = creatureCard.toughness

  def isTapped: Boolean = (state & TappedBitMask) == 1
  def isAttacking: Boolean = (state & AttackingBitMask) == AttackingBitMask
  def blockedId: Int = (state >> BlockingBitOffset) - 1
  def isBlocking: Boolean = blockedId >= 0

  def attack(tap: Boolean = true): Creature = tap match {
    case true => Creature(creatureCard, state | AttackingBitMask | TappedBitMask)
    case false => Creature(creatureCard, state | AttackingBitMask)
  }

  def block(blockedId: Int): Creature = {
    require(blockedId >= 0)

    Creature(
      creatureCard,
      ((blockedId + 1) << BlockingBitOffset) | (state & ((1 << BlockingBitOffset) - 1))
    )
  }

  def removeFromCombat() = Creature(creatureCard, state & TappedBitMask)

  def tap() = Creature(creatureCard, state | TappedBitMask)

  def untap() = Creature(creatureCard, state & ~TappedBitMask)


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
