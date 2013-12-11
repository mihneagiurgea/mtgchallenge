package test.scala

import org.scalatest._
import main.scala.Creature
import main.scala.CreatureCard
import main.scala.Battleground
import main.scala.GameState
import main.scala.Outcome
import main.scala.TurnPhase

class GameStateSpec extends FlatSpec {

  val c1 = Creature(CreatureCard(1, 1))
  val c2 = Creature(CreatureCard(2, 2))
  val c3 = Creature(CreatureCard(3, 3), tapped=true)

  val battleground =
      Battleground().addCreature(c1, 1).addCreature(c2, 1).addCreature(c3, 2)

  val gameState = GameState(battleground=battleground)

  "GameState" should "return equivalent instances for same arguments only" in {
    assert(GameState() === GameState())

    assert(
      GameState(battleground=battleground) ===
      GameState(battleground=battleground)
    )
  }

  it should "serialize and deserialize objects" in {
    assertIsSerializable(gameState)

    assertIsSerializable(GameState())

    assertIsSerializable(GameState(life1 = -3, life2 = 0))
  }

  it should "know when the game is over and the outcome" in {
    assert(!gameState.isOver)
    assert(gameState.outcome === Outcome.NotOver)

    assert(GameState(0, 20).isOver)
    assert(GameState(0, 20).outcome === Outcome.Loss)

    assert(GameState(1, -2).isOver)
    assert(GameState(1, -2).outcome === Outcome.Win)

    assert(GameState(1, -2, turnPhase=TurnPhase.DeclareBlockers).isOver)
    assert(GameState(1, -2, turnPhase=TurnPhase.DeclareBlockers).outcome === Outcome.Loss)

    assert(GameState(1, -2, activePlayer = 2).isOver)
    assert(GameState(1, -2, activePlayer = 2).outcome === Outcome.Loss)

    assert(GameState(0, 0).isOver)
    assert(GameState(0, 0).outcome === Outcome.Draw)
  }

  it should "end the current turn and move to the next one" in {
    val gameState = GameState(
      activePlayer = 1,
      battleground = Battleground.fromString("2/3 (T), 4/6 (T) vs 1/1 (T)"))
    val expectedGameState = GameState(
      activePlayer = 2,
      battleground = Battleground.fromString("2/3 (T), 4/6 (T) vs 1/1"))

    assert(gameState.endCurrentTurn() === expectedGameState)
  }

  def assertIsSerializable(gameState: GameState): Unit =
    assert(gameState === GameState.fromString(gameState.toString))

}
