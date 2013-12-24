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

  def apply(controller: Int, index: Int): Creature = this(controller)(index)

  def apply(controller: Int): List[Creature] = controller match {
    case 1 => player1
    case 2 => player2
  }

  def filterWithIndex(
      controller: Int, p: (Creature) => Boolean): List[(Creature, Int)] =
    this(controller).zipWithIndex.filter({ case (creature, _) => p(creature) })

  private def insertSorted[T <% Ordered[T]](ls: List[T], x: T): List[T] = {
    val (prefix, suffix) = ls.span(_ < x)
    prefix ::: x :: suffix
  }

  def addCreature(creature: Creature, controller: Int): Battleground = controller match {
    case 1 => Battleground(insertSorted(player1, creature), player2)
    case 2 => Battleground(player1, insertSorted(player2, creature))
  }

  def removeMany(indexes1: Seq[Int], indexes2: Seq[Int]): Battleground = {
    val set1 = indexes1.toSet
    val set2 = indexes2.toSet
    Battleground(
      player1.zipWithIndex.filter(x => !set1.contains(x._2)).map(_._1),
      player2.zipWithIndex.filter(x => !set2.contains(x._2)).map(_._1))
  }

  /* TurnPhase-related logic */

  def untap(controller: Int): Battleground = controller match {
    case 1 => Battleground(player1.map(_.untap), player2)
    case 2 => Battleground(player1, player2.map(_.untap))
  }

  def declareAttackers(
      controller: Int, indexes: Set[Int]): Battleground = controller match {
    case 1 => Battleground(attack(player1, indexes), player2)
    case 2 => Battleground(player1, attack(player2, indexes))
  }

  def declareBlockers(
      controller: Int, blockingAssignment: Map[Int, Int]): Battleground = controller match {
    case 1 => Battleground(block(player1, blockingAssignment), player2)
    case 2 => Battleground(player1, block(player2, blockingAssignment))
  }

  // TODO - improve performance my merging this with removeMany usage
  def removeAllFromCombat(): Battleground =
    Battleground(
      player1.map(_.removeFromCombat()),
      player2.map(_.removeFromCombat()))

  private def attack(
      creatures: List[Creature], indexes: Set[Int]): List[Creature] =
    creatures.zipWithIndex.map(
      { case (creature, idx) =>
          if (indexes.contains(idx)) creature.attack() else creature })

  private def block(
      creatures: List[Creature], blockingAssignment: Map[Int, Int]): List[Creature] =
    creatures.zipWithIndex.map(
      { case (creature, idx) =>
          if (blockingAssignment.contains(idx)) creature.block(blockingAssignment(idx))
          else creature })

  override def toString = s"${player1.mkString(", ")} vs ${player2.mkString(", ")}"
}
