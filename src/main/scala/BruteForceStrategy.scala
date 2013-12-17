package main.scala

import TurnPhase._

case class BruteForceStrategy() {

  def getNextStates(
      gameState: GameState): Iterator[GameState] = gameState.turnPhase match {
    case TurnPhase.DeclareAttackers => getNextStatesWhenAttacking(gameState)
  }

  private def getNextStatesWhenAttacking(
      gameState: GameState): Iterator[GameState] = {
    val creaturesWithIndex = gameState.attackingPlayerCreatures.zipWithIndex
    val canAttackWithIndex = creaturesWithIndex.filter(x => !x._1.isTapped)
    val canAttackIndexes = canAttackWithIndex.map(_._2).toSet
    for (subset <- canAttackIndexes.subsets)
      yield gameState.declareAttackers(subset.toList)
  }

}
