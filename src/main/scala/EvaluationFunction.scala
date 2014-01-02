package main.scala

object EvaluationFunction {

  val lifeWeight = 10.0
  val creaturePowerWeight = 1.2

  def apply(gameState: GameState): Double = {
    // Compute score for player1, then negate if nextToAct is player2.
    val livesScore = (gameState.life1 - gameState.life2) * lifeWeight
    val battleground = gameState.battleground
    val bgScore = apply(battleground.player1) - apply(battleground.player2)

    val score = livesScore + bgScore
    if (gameState.nextToAct == 1) score
    else -score
  }

  def apply(playerBattleground: PlayerBattleground): Double =
    playerBattleground.creatures.map(apply).sum

  def apply(creature: Creature): Double =
    creature.power * creaturePowerWeight + creature.toughness

}
