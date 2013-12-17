package main.scala

import scala.collection.mutable.MutableList
import scala.util.matching.Regex
import Outcome._
import TurnPhase._

/** Factory for GameState instances. */
object GameState {

  private val regex = new Regex("""(-?\d+)/(-?\d+) \((\d)/(.*)\): (.*)""")

  def fromString(s: String): GameState = {
    val regex(life1, life2, activePlayer, turnPhase, battleground) = s

    GameState(
      life1.toInt,
      life2.toInt,
      activePlayer.toInt,
      TurnPhase.withName(turnPhase),
      Battleground.fromString(battleground))
  }

}

/** The entire game state, encoding players' lives, turn structure and
  * all the state of all creatures.
  *
  * Should be constructed using the companion object.
  */
case class GameState(
    life1: Int = 20,
    life2: Int = 20,
    activePlayer: Int = 1,
    turnPhase: TurnPhase = TurnPhase.DeclareAttackers,
    battleground: Battleground = Battleground()) {

  require(activePlayer == 1 || activePlayer == 2)

  def attackingPlayer: Int = activePlayer
  def defendingPlayer: Int = 3 - activePlayer
  def nextToAct: Int =
    if (turnPhase == TurnPhase.DeclareBlockers) defendingPlayer
    else attackingPlayer

  def isOver: Boolean = life1 <= 0 || life2 <= 0

  def outcome: Outcome =
    if (!isOver) Outcome.NotOver
    else
      if (life1 <= 0 && life2 <= 0) Outcome.Draw
      else {
        val deadPlayer = if (life1 <= 0) 1 else 2
        if (nextToAct == deadPlayer) Outcome.Loss
        else Outcome.Win
      }

  /* Convenience methods */

  def filterAttackingPlayerCreaturesWithIndex(
      p: (Creature) => Boolean): List[(Creature, Int)] =
    battleground.filterWithIndex(attackingPlayer, p)

  def filterDefendingPlayerCreaturesWithIndex(
      p: (Creature) => Boolean): List[(Creature, Int)] =
    battleground.filterWithIndex(defendingPlayer, p)

  def filterAttackingPlayerCreatureIndexes(p: (Creature) => Boolean): List[Int] =
    battleground.filterWithIndex(attackingPlayer, p).map(_._2)

  def filterDefendingPlayerCreatureIndexes(p: (Creature) => Boolean): List[Int] =
    battleground.filterWithIndex(defendingPlayer, p).map(_._2)

  def attackingPlayerCreatures = battleground(attackingPlayer)
  def defendingingPlayerCreatures = battleground(defendingPlayer)

  override def toString = s"$life1/$life2 ($activePlayer/$turnPhase): $battleground"

  def isValidAttack(attackingCreatureUids: List[Int]): Boolean =
    attackingCreatureUids.forall(
      uid => !battleground(attackingPlayer, uid).isTapped)

  def isValidBlock(blockingAssignment: Map[Int, Int]): Boolean =
    blockingAssignment.forall({ case (blockerIdx, blockedIdx) =>
      !battleground(defendingPlayer, blockerIdx).isTapped &&
      battleground(attackingPlayer, blockedIdx).isAttacking
    })

  def combatAssignment: Map[Int, List[Int]] = {
    val blockers = battleground(defendingPlayer).zipWithIndex.filter(_._1.isBlocking)
    val groupedWithIndex = blockers.groupBy(_._1.blockedId)
    val grouped = groupedWithIndex.map({ case (k, v) => (k, v.map(_._2)) })

    val attackerIndexes = battleground(attackingPlayer).zipWithIndex.
      filter(_._1.isAttacking).map(_._2)
    attackerIndexes.filter(!grouped.contains(_)).foldLeft(grouped)(
      (map, idx) => map.updated(idx, List[Int]()))
  }

  /* State-altering methods */

  def declareAttackers(attackingCreatureUids: List[Int]): GameState = {
    if (!isValidAttack(attackingCreatureUids) ||
        turnPhase != TurnPhase.DeclareAttackers)
      throw new IllegalArgumentException
    attackingCreatureUids match {
      case Nil => endCurrentTurn()
      case _ =>
        GameState(
          life1,
          life2,
          attackingPlayer,
          TurnPhase.DeclareBlockers,
          battleground.declareAttackers(attackingPlayer, attackingCreatureUids.toSet))
    }
  }

  def declareBlockers(blockingAssignment: Map[Int, Int]): GameState = {
    if (!isValidBlock(blockingAssignment) ||
        turnPhase != TurnPhase.DeclareBlockers)
      throw new IllegalArgumentException
    GameState(
      life1,
      life2,
      activePlayer,
      TurnPhase.CombatStep,
      battleground.declareBlockers(defendingPlayer, blockingAssignment))
  }

  // TODO - make combatAssignment a List instead of a Map?
  def resolveCombat(combatAssignment: Map[Int, List[Int]]): GameState = {
    // TODO - validate combatAssignment?

    // Breaking out of pure functional style. TODO - ƒix this.
    var defenderDamage = 0
    var deadAttackers = MutableList[Int]()
    var deadBlockers = MutableList[Int]()

    for ( (attackerIdx, blockers) <- combatAssignment ) {
      val attacker = battleground(attackingPlayer)(attackerIdx)
      if (blockers.length == 0)
        defenderDamage += attacker.power
      else {
        var attackerPower = attacker.power
        var blockersTotalPower = 0

        for (blockerIdx <- blockers) {
          val blocker = battleground(defendingPlayer)(blockerIdx)

          blockersTotalPower += blocker.power
          if (attackerPower >= blocker.toughness) {
            deadBlockers += blockerIdx
            attackerPower -= blocker.toughness
          }
        }

        // Will the attacker die?
        if (blockersTotalPower >= attacker.toughness)
          deadAttackers += attackerIdx
      }
    }

    GameState(
      life1,
      life2 - defenderDamage,
      defendingPlayer,
      DeclareAttackers,
      battleground.removeMany(deadAttackers, deadBlockers).removeAllFromCombat())
  }

  def endCurrentTurn(): GameState =
    GameState(
      life1,
      life2,
      defendingPlayer,
      TurnPhase.DeclareAttackers,
      battleground.untap(defendingPlayer))


}
