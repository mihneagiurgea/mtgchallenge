package main.scala

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
      (map, idx) => if (map.contains(idx)) map else map.updated(idx, List[Int]()))
  }


  /* State-altering methods */

  def declareAttackers(attackingCreatureUids: List[Int]): GameState = {
    if (!isValidAttack(attackingCreatureUids) ||
        turnPhase != TurnPhase.DeclareAttackers)
      throw new IllegalArgumentException
    GameState(
      life1,
      life2,
      activePlayer,
      TurnPhase.DeclareBlockers,
      battleground.declareAttackers(attackingPlayer, attackingCreatureUids.toSet))
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

  def endCurrentTurn(): GameState =
    GameState(
      life1,
      life2,
      defendingPlayer,
      TurnPhase.DeclareAttackers,
      battleground.untap(defendingPlayer))


}
