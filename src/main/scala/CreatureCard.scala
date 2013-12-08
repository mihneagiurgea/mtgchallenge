package main.scala

/** A stateless CreatureCard.
  *
  * Should be constructed using the companion object.
  */
class CreatureCard private(val power: Int, val toughness: Int) {

  require(power >= 0)
  require(toughness >= 0)

  override def toString = s"$power/$toughness"
}

/** Factory for CreatureCard instances. */
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
