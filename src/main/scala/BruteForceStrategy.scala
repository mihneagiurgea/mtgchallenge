package main.scala

import Combinatorics._
import TurnPhase._

case class BruteForceStrategy() extends GameGraph[GameState] {

  private val NO_BLOCK_INDEX = -1

  def getNextStates(
      gameState: GameState): Set[GameState] = gameState.turnPhase match {
    case TurnPhase.DeclareAttackers => getNextStatesWhenAttacking(gameState).toSet
    case TurnPhase.DeclareBlockers => getNextStatesWhenBlocking(gameState).toSet
    case TurnPhase.CombatStep => getNextStatesDuringCombatStep(gameState).toSet
  }

  private def getNextStatesWhenAttacking(
      gameState: GameState): Iterator[GameState] = {
    val canAttackIndexes =
      gameState.filterAttackingPlayerCreatureIndexes(!_.isTapped).toSet
    for (subset <- canAttackIndexes.subsets)
      yield gameState.declareAttackers(subset)
  }

  private def getNextStatesWhenBlocking(
      gameState: GameState): Iterator[GameState] = {
    val attackingIndexes =
      gameState.filterAttackingPlayerCreatureIndexes(_.isAttacking)
    val canDefendIndexes =
      gameState.filterDefendingPlayerCreatureIndexes(!_.isTapped)

    // Generate all possible blocking assignments, by adding a "-1" index
    // marking a fake attacker (representing a "no block").
    val mappings = Combinatorics.getAllMappings(
      canDefendIndexes,
      NO_BLOCK_INDEX :: attackingIndexes)
    for (mapping <- mappings.toIterator) yield {
      // Remove NO_BLOCK_UID from mapping.
      val fixedMapping = mapping.filterNot( { case (k, value) => value == -1 } )
      gameState.declareBlockers(fixedMapping)
    }
  }

  private def getNextStatesDuringCombatStep(
      gameState: GameState): Iterator[GameState] = {
    val unorderedCombatAssignment = gameState.combatAssignment
    val mappings =
      Combinatorics.getAllShuffledMappings(unorderedCombatAssignment)
    for (mapping <- mappings.toIterator)
      yield gameState.resolveCombat(mapping)
  }

}
