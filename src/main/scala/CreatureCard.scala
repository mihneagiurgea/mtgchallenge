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
  */
class CreatureCard private(val power: Int, val toughness: Int)
  extends Ordered[CreatureCard] {

  require(power >= 0)
  require(toughness >= 0)

  def compare(that: CreatureCard) = {
    val this_size = power + toughness
    val that_size = that.power + that.toughness

    if (this_size == that_size) this.power - that.power
    else this_size - that_size
  }

  override def toString = s"$power/$toughness"
}
