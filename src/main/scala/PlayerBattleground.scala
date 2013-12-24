package main.scala

/** Factory for PlayerBattleground instances. */
object PlayerBattleground {

  def apply(creatures: Creature*): PlayerBattleground = {
    val z = new PlayerBattleground()
    creatures.foldLeft(z)(_ + _)
  }

  def fromString(s: String): PlayerBattleground = {
    val split = s.trim.split(", ").toList.map(_.trim).filter(_.length > 0)
    PlayerBattleground(split.map(Creature.fromString(_)))
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

  def declareAttackers(indexes: Set[Int]): PlayerBattleground =
    PlayerBattleground(
      creatures.zipWithIndex.map(
        { case (creature, idx) =>
            if (indexes(idx)) creature.attack() else creature })
    )

  def declareBlockers(blockingAssignment: Map[Int, Int]): PlayerBattleground =
    PlayerBattleground(
      creatures.zipWithIndex.map(
        { case (creature, idx) =>
            if (blockingAssignment.contains(idx)) creature.block(blockingAssignment(idx))
            else creature })
    )

  def removeMany(indexes: Set[Int]): PlayerBattleground =
    PlayerBattleground(
      creatures.zipWithIndex.withFilter(x => !indexes(x._2)).map(_._1))

  /* The following methods mimic a List-like behaviour. */
  def map(f: Creature => Creature): PlayerBattleground =
    PlayerBattleground(creatures.map(f))

  override def toString = creatures.mkString(", ")

}
