package main.scala

/** Factory for PlayerBattleground instances. */
object PlayerBattleground {

  // The current implementation of PlayerBattleground needs some Ordering
  // of creatures. We'll define it here because other clients should NOT
  // use it.
  object SizeOrdering extends Ordering[Creature] {

    def compare(a: Creature, b: Creature) = {
      val cmpCreatureCard = compare(a.creatureCard, b.creatureCard)
      if (cmpCreatureCard == 0) a.state compare b.state
      else cmpCreatureCard
    }

    def compare(a: CreatureCard, b: CreatureCard) = {
      // Ordered by power + toughness, then power. E.g.: 3/3 < 4/4, 1/5 < 3/3
      val sizeA = a.power + a.toughness
      val sizeB = b.power + b.toughness

      if (sizeA == sizeB) a.power compare b.power
      else sizeA compare sizeB
    }

  }

  def apply(creatures: Creature*): PlayerBattleground =
    fromUnsortedCreatures(creatures.toList)

  def fromString(s: String): PlayerBattleground = {
    val split = s.trim.split(", ").toList.map(_.trim).filter(_.length > 0)
    val creatures = split.map(Creature.fromString(_))
    // We cannot use PlayerBattleground(creatures) because that will
    // use the private apply() corresponding to the case class' default
    // constructor.
    fromUnsortedCreatures(creatures)
  }

  protected def fromUnsortedCreatures(
      creatures: List[Creature]): PlayerBattleground = {
    val z = new PlayerBattleground()
    creatures.foldLeft(z)(_ + _)
  }

}

/** A collection for all Creatures under some player's control.
  *
  * Should be constructed using the companion object.
  * Defines a "strictly better than" partial ordering.
  */
case class PlayerBattleground private(creatures: List[Creature])
  extends PartiallyOrdered[PlayerBattleground]{

  import PlayerBattleground.SizeOrdering

  private def this() = this(Nil)

  def tryCompareTo[B >: PlayerBattleground](that: B)
      (implicit arg0: (B) ⇒ PartiallyOrdered[B]) =
    tryCompareTo(that.asInstanceOf[PlayerBattleground])

  def tryCompareTo(that: PlayerBattleground): Option[Int] = {

    def f(x: List[Creature], y: List[Creature], res: Option[Int]): Option[Int] = (x, y) match {
      case (Nil, Nil) => res
      case (Nil, _) => StrictlyBetter.combine(res, Some(-1))
      case (_, Nil) => StrictlyBetter.combine(res, Some(+1))
      case (h1 :: t1, h2 :: t2) => {
        val newRes = StrictlyBetter.combine(res, h1.tryCompareTo(h2))
        newRes match {
          case None => None
          case _ => f(t1, t2, newRes)
        }
      }
    }
    f(this.creatures, that.creatures, Some(0))
  }

  def + (creature: Creature): PlayerBattleground = {
    val (prefix, suffix) = creatures.span(SizeOrdering.lt(_, creature))
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
