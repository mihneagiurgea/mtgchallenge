package main.scala

/** Factory for Battleground instances. */
object Battleground {

  def apply(): Battleground = new Battleground(PlayerBattleground(), PlayerBattleground())

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
case class Battleground private(player1: PlayerBattleground, player2: PlayerBattleground) {

  def addCreature(creature: Creature, controller: Int): Battleground = controller match {
    case 1 => Battleground(player1 + creature, player2)
    case 2 => Battleground(player1, player2 + creature)
  }

  def removeAt(index: Int, controller: Int): Battleground = controller match {
    case 1 => Battleground(player1.removeAt(index), player2)
    case 2 => Battleground(player1, player2. removeAt(index))
  }

  def creatures(controller: Int): List[Creature] = controller match {
    case 1 => player1.creatures
    case 2 => player2.creatures
  }

  override def toString = s"$player1 vs $player2"
}
