package main.scala

import main.scala.StrictlyBetter._

object MinimaxStrategy {

  def apply(): MinimaxStrategy = new MinimaxStrategy()

}

class MinimaxStrategy() extends BruteForceStrategy {

  // TODO - performance can be improved by avoiding duplicate computation
  // (either via caching or better design).

  override protected def getNextStatesWhenAttacking(
      gameState: GameState): Set[GameState] = {
    val noAttackers = gameState.declareAttackers(Set())

    // Current improvement only drop attacks which prove to be inefficient
    // this turn. We can further improve this.
    val declareBlockersNextStates = super.getNextStatesWhenAttacking(gameState)
    declareBlockersNextStates.filterNot(
      state => {
        // If this attack yields a single state after DeclareBlockers
        // and CombatStep AND if that state is worse than not attacking
        // at all, then ignore this state.
        if (state == noAttackers) false
        else {
          val statesAfterBlocking = getNextStatesWhenBlocking(state)
          if (statesAfterBlocking.size != 1) false
          else {
            val statesAfterCombat =
              statesAfterBlocking.flatMap(getNextStatesDuringCombatStep)
            // ">" instead of "<" because zz.head will be evaluated
            // from the other player's perspective
            statesAfterCombat.size == 1 && statesAfterCombat.head > noAttackers
          }
        }
      })
  }

  /** Return only the best next states, with regards to the StriclyBetter
    * partial ordering.
    */
  override protected def getNextStatesWhenBlocking(
      gameState: GameState): Set[GameState] = {
    val combatStepNextStates = super.getNextStatesWhenBlocking(gameState)

    // For each next state `s` in next CombatStep states, compute
    // successor(s) and map the resulting Set[GameState] to its parent node.
    val parents: Map[Set[GameState], GameState] =
      combatStepNextStates.map(s => (successor(s) -> s)).toMap

    // Compute the maximals from all (DeclareAttackers) states from parents,
    // then return the moves (block) which yield those maximals.
    val maximals = StrictlyBetter.maximals(parents.keySet)
    maximals.map(parents)
  }

  override protected def getNextStatesDuringCombatStep(
      gameState: GameState): Set[GameState] = {
    // Only consider the minimal states out of all possible ways of reordering
    // the blocking creatures.
    StrictlyBetter.minimals(super.getNextStatesDuringCombatStep(gameState))
  }

}
