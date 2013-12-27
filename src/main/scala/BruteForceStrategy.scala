package main.scala

import Combinatorics._
import TurnPhase._

object BruteForceStrategy {

  def apply(): BruteForceStrategy = new BruteForceStrategy()

}

class BruteForceStrategy() extends GameGraph[GameState] {

  protected val NO_BLOCK_INDEX = -1

  def getNextStates(
      gameState: GameState): Set[GameState] = gameState.turnPhase match {
    case TurnPhase.DeclareAttackers => getNextStatesWhenAttacking(gameState)
    case TurnPhase.DeclareBlockers => getNextStatesWhenBlocking(gameState)
    case TurnPhase.CombatStep => getNextStatesDuringCombatStep(gameState)
  }

  protected def getNextStatesWhenAttacking(
      gameState: GameState): Set[GameState] = {
    val canAttackIndexes =
      gameState.filterAttackingPlayerCreatureIndexes(!_.isTapped).toSet
    // Build the powerset of canAttackIndexes (a set of all subsets).
    val canAttackIndexesPowerset = canAttackIndexes.subsets.toSet
    canAttackIndexesPowerset.map(subset => gameState.declareAttackers(subset))
  }

  protected def getNextStatesWhenBlocking(
      gameState: GameState): Set[GameState] = {
    val attackingIndexes =
      gameState.filterAttackingPlayerCreatureIndexes(_.isAttacking)
    val canDefendIndexes =
      gameState.filterDefendingPlayerCreatureIndexes(!_.isTapped)

    // Generate all possible blocking assignments, by adding a "-1" index
    // marking a fake attacker (representing a "no block").
    val mappings = Combinatorics.getAllMappings(
      canDefendIndexes,
      NO_BLOCK_INDEX :: attackingIndexes)
    // Remove NO_BLOCK_UID from mapping.
    val fixedMappings =
      mappings.map(mapping => mapping.filterNot( { case (_, v) => v == -1 } ))
    fixedMappings.map(mapping => gameState.declareBlockers(mapping)).toSet
  }

  protected def getNextStatesDuringCombatStep(
      gameState: GameState): Set[GameState] = {
    val unorderedCombatAssignment = gameState.combatAssignment
    val mappings =
      Combinatorics.getAllShuffledMappings(unorderedCombatAssignment)
    mappings.map(mapping => gameState.resolveCombat(mapping)).toSet
  }

}
