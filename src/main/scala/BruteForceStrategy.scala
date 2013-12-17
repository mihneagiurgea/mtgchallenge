package main.scala

import TurnPhase._

case class BruteForceStrategy() {

  def getNextStates(
      gameState: GameState): Iterator[GameState] = gameState.turnPhase match {
    case TurnPhase.DeclareAttackers => getNextStatesWhenAttacking(gameState)
    case TurnPhase.DeclareBlockers => getNextStatesDuringCombatStep(gameState)
    case TurnPhase.CombatStep => getNextStatesDuringCombatStep(gameState)
  }

  private def getNextStatesWhenAttacking(
      gameState: GameState): Iterator[GameState] = {
    val canAttackWithIndex =
      gameState.filterAttackingPlayerCreaturesWithIndex(!_.isTapped)
    // val creaturesWithIndex = gameState.attackingPlayerCreatures.zipWithIndex
    // val canAttackWithIndex = creaturesWithIndex.filter(x => !x._1.isTapped)
    val canAttackIndexes = canAttackWithIndex.map(_._2).toSet
    for (subset <- canAttackIndexes.subsets)
      yield gameState.declareAttackers(subset.toList)
  }

  private def getNextStatesWhenBlocking(
      gameState: GameState): Iterator[GameState] = {
    Iterator[GameState]()
  }

  private def getNextStatesDuringCombatStep(
      gameState: GameState): Iterator[GameState] = {
    Iterator[GameState]()
  }

/*

    def _get_next_states_when_blocking(self, state):
        # What creatures can block / what creatures are attacking?
        attacking_creature_uids = []
        blocking_creatures_uids = []
        for uid, creature in state.battleground.creatures_with_uids:
            if (creature.controlling_player == state.defending_player and
                    not creature.tapped):
                blocking_creatures_uids.append(uid)
            elif creature.attacking:
                attacking_creature_uids.append(uid)
        # Generate all possible blocking assignments, by adding a "0" uid
        # marking a fake attacker (representing a "no block").
        attacking_creature_uids.append(NO_BLOCK_UID)
        mappings_generator = \
            combinatorics.get_all_mappings(blocking_creatures_uids,
                                           attacking_creature_uids)
        for mapping in mappings_generator:
            # Remove NO_BLOCK_UID from mapping.
            for key in mapping.keys():
                if mapping[key] == NO_BLOCK_UID:
                    del mapping[key]
            next_state = state.copy()
            next_state.declare_blockers(mapping)
            yield next_state

*/


}
