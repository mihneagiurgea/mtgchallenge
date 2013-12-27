package main.scala

/** Factory for CreatureCard instances - NOT THREAD SAFE. */
object CreatureCard {

  protected var cache = collection.mutable.Map[(Int, Int), CreatureCard]()

  def apply(power: Int, toughness: Int) = {
    val key = (power, toughness)
    if (!(cache contains key)) {
      val instance = new CreatureCard(power, toughness)
      cache.update(key, instance)
    }
    cache(key)
  }

  def fromString(s: String) = {
    val split = s.split('/').map(_.toInt)
    CreatureCard(split(0), split(1))
  }
}

/** A stateless CreatureCard.
  *
  * Should be constructed using the companion object.
  * Defines an arbitrary total ordering and a "strictly better than" partial
  * ordering.
  */
class CreatureCard private(val power: Int, val toughness: Int)
  extends PartiallyOrdered[CreatureCard] {

  require(power >= 0)
  require(toughness >= 0)

  def tryCompareTo[B >: CreatureCard](that: B)
      (implicit arg0: (B) ⇒ PartiallyOrdered[B]) =
    tryCompareTo(that.asInstanceOf[CreatureCard])

  def tryCompareTo(that: CreatureCard): Option[Int] =
    StrictlyBetter.combine(this.power - that.power,
      this.toughness - that.toughness)

  override def toString = s"$power/$toughness"
}
