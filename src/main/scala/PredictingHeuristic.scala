package main.scala

import Outcome._

/** A PredictingHeuristic tries to predict the outcome of non-leaf GameStates.
  *
  */
object PredictingHeuristic {

  private def hasCreatures(gameState: GameState, player: Int): Boolean =
    gameState.battleground(player).length > 0

  private def hasCreaturesWithPower(gameState: GameState, player: Int): Boolean =
    gameState.battleground.filter(player, _.power > 0).length > 0

  def predictOutcome(gameState: GameState): Outcome = {
    if (gameState.isLeaf) gameState.outcome
    else {
      // For each player, determine if he has any creatures, and if he has
      // any creatures that can attack (power > 0).
      val meCreatures = hasCreatures(gameState, gameState.nextToAct)
      val meCanAttack = hasCreaturesWithPower(gameState, gameState.nextToAct)
      val oppCreatures = hasCreatures(gameState, gameState.secondToAct)
      val oppCanAttack = hasCreaturesWithPower(gameState, gameState.secondToAct)

      (meCreatures, meCanAttack, oppCreatures, oppCanAttack) match {
        // Case 1 - nobody has any creatures.
        case (false, false, false, false) => Outcome.Draw

        // Case 2 - only one player has creatures and they can also attack.
        case (true, true, false, false) => Outcome.Win
        case (false, false, true, true) => Outcome.Loss

        // Case 3 - only one player has creatures but they cannot attack.
        case (true, false, false, false) => Outcome.Draw
        case (false, false, true, false) => Outcome.Draw

        // Case 4 - both players have creatures, but none can attack.
        case (true, false, true, false) => Outcome.Draw

        // Otherwise, we can't make any decision.
        case _ => Outcome.NotOver
      }
    }

  }

}
