package main.scala

/** Factory for PlayerBattleground instances. */
object PlayerBattleground {

  def apply(creatures: Creature*): PlayerBattleground = {
    val z = new PlayerBattleground()
    creatures.foldLeft(z)(_ + _)
  }

}

/** A collection for all Creatures under some player's control.
  *
  * Should be constructed using the companion object.
  */
case class PlayerBattleground private(creatures: List[Creature]) {

  private def this() = this(Nil)

  def + (creature: Creature): PlayerBattleground = {
    val (prefix, suffix) = creatures.span(_ < creature)
    PlayerBattleground(prefix ::: creature :: suffix)
  }

  def removeAt(index: Int): PlayerBattleground =
    PlayerBattleground(creatures.take(index) ::: creatures.drop(index + 1))

}
