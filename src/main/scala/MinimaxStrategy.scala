package main.scala

import main.scala.StrictlyBetter._

object MinimaxStrategy {

  def apply(): MinimaxStrategy = new MinimaxStrategy()

}

class MinimaxStrategy() extends BruteForceStrategy {

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
