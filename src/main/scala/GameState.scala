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

}
