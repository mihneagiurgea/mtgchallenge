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
    battleground: Battleground = Battleground())
  extends GameNode
  with PartiallyOrdered[GameState] {

  require(activePlayer == 1 || activePlayer == 2)

  def attackingPlayer: Int = activePlayer
  def defendingPlayer: Int = 3 - activePlayer
  def nextToAct: Int =
    if (turnPhase == TurnPhase.DeclareBlockers) defendingPlayer
    else attackingPlayer
  def secondToAct: Int = 3 - nextToAct

  def isOver: Boolean = life1 <= 0 || life2 <= 0
  def isLeaf: Boolean = isOver

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

  /* Business logic */

  def tryCompareTo[B >: GameState](that: B)
      (implicit arg0: (B) ⇒ PartiallyOrdered[B]) =
    tryCompareTo(that.asInstanceOf[GameState])

  def tryCompareTo(that: GameState): Option[Int] = {
    // In order for GameStates to be comparable, they must have the same
    // TurnPhase and active player.
    if (this.activePlayer != that.activePlayer ||
      this.turnPhase != that.turnPhase) None
    else
      StrictlyBetter.combine(
        tryCompareLives(that),
        tryCompareBattlegrounds(that.battleground)
      )
  }

  private def tryCompareLives(that: GameState): Option[Int] = {
    val cmpLife1 = this.life1 - that.life1
    val cmpLife2 = this.life2 - that.life2
    if (nextToAct == 1) StrictlyBetter.combine(cmpLife1, -cmpLife2)
    else StrictlyBetter.combine(-cmpLife1, cmpLife2)
  }

  private def tryCompareBattlegrounds(that: Battleground): Option[Int] = {
    val cmpPlayer1 = battleground.player1.tryCompareTo(that.player1)
    if (cmpPlayer1.isEmpty) None
    else {
      val cmpPlayer2 = battleground.player2.tryCompareTo(that.player2)
      if (cmpPlayer2.isEmpty) None
      else
        if (nextToAct == 1)
          StrictlyBetter.combine(cmpPlayer1.get, -cmpPlayer2.get)
        else
          StrictlyBetter.combine(-cmpPlayer1.get, cmpPlayer2.get)
    }
  }

  def isValidAttack(attackingCreatureUids: Set[Int]): Boolean =
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

  def declareAttackers(attackingCreatureUids: Set[Int]): GameState = {
    if (!isValidAttack(attackingCreatureUids) ||
        turnPhase != TurnPhase.DeclareAttackers)
      throw new IllegalArgumentException
    if (attackingCreatureUids.isEmpty)
      endCurrentTurn()
    else
      GameState(
        life1,
        life2,
        attackingPlayer,
        TurnPhase.DeclareBlockers,
        battleground.declareAttackers(attackingPlayer, attackingCreatureUids))
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
    val deadAttackers = MutableList[Int]()
    val deadBlockers = MutableList[Int]()

    for ( (attackerIdx, blockers) <- combatAssignment ) {
      val attacker = battleground(attackingPlayer)(attackerIdx)
      if (blockers.length == 0)
        // Unblocked attacked, deal damage to defending player.
        defenderDamage += attacker.power
      else {
        // Blocked attacked, deal damage to defending creatures, in order.
        var attackerPower = attacker.power
        var blockersTotalPower = 0

        for (blockerIdx <- blockers) {
          val blocker = battleground(defendingPlayer)(blockerIdx)

          blockersTotalPower += blocker.power
          attackerPower -= blocker.toughness
          if (attackerPower >= 0)
            deadBlockers += blockerIdx
        }

        // Will the attacker die?
        if (blockersTotalPower >= attacker.toughness)
          deadAttackers += attackerIdx
      }
    }

    // Determine which player takes damage and what creatures die, depending
    // on attacking player.
    val (damage1, damage2, deadCreatures1, deadCreatures2) =
      if (attackingPlayer == 1)
        (0, defenderDamage, deadAttackers, deadBlockers)
      else
        (defenderDamage, 0, deadBlockers, deadAttackers)
    GameState(
      life1 - damage1,
      life2 - damage2,
      defendingPlayer,
      DeclareAttackers,
      battleground.removeMany(deadCreatures1.toSet, deadCreatures2.toSet).
        removeAllFromCombat().untap(defendingPlayer))
  }

  def endCurrentTurn(): GameState =
    GameState(
      life1,
      life2,
      defendingPlayer,
      TurnPhase.DeclareAttackers,
      battleground.untap(defendingPlayer))


}
