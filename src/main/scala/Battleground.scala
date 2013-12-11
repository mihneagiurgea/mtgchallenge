package main.scala

/** Factory for Battleground instances. */
object Battleground {

  def fromString(s: String): Battleground = {
    val split = s.split("vs")

    def parseCreatures(s: String): List[Creature] = {
      val split = s.trim.split(", ").toList.map(_.trim).filter(_.length > 0)
      split.map(Creature.fromString(_))
    }

    Battleground(parseCreatures(split(0)), parseCreatures(split(1)))
  }

}

/** A collection for all Creatures on the battleground (under any player's control).
  *
  * Should be constructed using the companion object.
  */
case class Battleground(
    player1: List[Creature] = List[Creature](),
    player2: List[Creature] = List[Creature]()) {

  private def insertSorted[T <% Ordered[T]](ls: List[T], x: T): List[T] = {
    val (prefix, suffix) = ls.span(_ < x)
    prefix ::: x :: suffix
  }

  private def removeAt[T](ls: List[T], index: Int): List[T] =
    ls.take(index) ::: ls.drop(index + 1)

  def addCreature(creature: Creature, controller: Int): Battleground = controller match {
    case 1 => Battleground(insertSorted(player1, creature), player2)
    case 2 => Battleground(player1, insertSorted(player2, creature))
  }

  def removeAt(index: Int, controller: Int): Battleground = controller match {
    case 1 => Battleground(removeAt(player1, index), player2)
    case 2 => Battleground(player1, removeAt(player2, index))
  }

  def creatures(controller: Int): List[Creature] = controller match {
    case 1 => player1
    case 2 => player2
  }

  override def toString = s"${player1.mkString(", ")} vs ${player2.mkString(", ")}"
}
