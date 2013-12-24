package main.scala

/** Factory for Battleground instances. */
object Battleground {

  def fromString(s: String): Battleground = {
    val split = s.split("vs")

    Battleground(
      PlayerBattleground.fromString(split(0)),
      PlayerBattleground.fromString(split(1))
    )
  }

}

/** A collection for all Creatures on the battleground (under any player's control).
  *
  * Should be constructed using the companion object.
  */
case class Battleground(
    player1: PlayerBattleground = PlayerBattleground(),
    player2: PlayerBattleground = PlayerBattleground()) {

  def apply(controller: Int, index: Int): Creature = this(controller)(index)

  def apply(controller: Int): List[Creature] = controller match {
    case 1 => player1.creatures
    case 2 => player2.creatures
  }

  def filterWithIndex(
      controller: Int, p: (Creature) => Boolean): List[(Creature, Int)] =
    this(controller).zipWithIndex.filter({ case (creature, _) => p(creature) })

  def addCreature(creature: Creature, controller: Int): Battleground = controller match {
    case 1 => Battleground(player1 + creature, player2)
    case 2 => Battleground(player1, player2 + creature)
  }

  def removeMany(indexes1: Set[Int], indexes2: Set[Int]): Battleground =
    Battleground(
      player1.removeMany(indexes1.toSet),
      player2.removeMany(indexes2.toSet))

  /* TurnPhase-related logic */

  def untap(controller: Int): Battleground = controller match {
    case 1 => Battleground(player1.map(_.untap), player2)
    case 2 => Battleground(player1, player2.map(_.untap))
  }

  def declareAttackers(
      controller: Int, indexes: Set[Int]): Battleground = controller match {
    case 1 => Battleground(player1.declareAttackers(indexes), player2)
    case 2 => Battleground(player1, player2.declareAttackers(indexes))
  }

  def declareBlockers(
      controller: Int, blockingAssignment: Map[Int, Int]): Battleground = controller match {
    case 1 => Battleground(player1.declareBlockers(blockingAssignment), player2)
    case 2 => Battleground(player1, player2.declareBlockers(blockingAssignment))
  }

  // TODO - improve performance my merging this with removeMany usage
  def removeAllFromCombat(): Battleground =
    Battleground(
      player1.map(_.removeFromCombat),
      player2.map(_.removeFromCombat)
    )

  override def toString = s"$player1 vs $player2"
}
