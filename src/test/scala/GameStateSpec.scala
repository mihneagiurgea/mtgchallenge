package test.scala

import org.scalatest.FlatSpec
import main.scala._

class GameStateSpec extends FlatSpec {

  // TODO - the current usage of Battleground.fromString is a crude hack!
  // E.g.: Battleground.fromString("1/1 (TA), 2/2 (TA) vs 3/3 (B#0), 4/4 (B#0)")
  // the order of the creatures is hardcoded in the string & in the blocking
  // assignments.

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

    def assertIsSerializable(gameState: GameState): Unit =
      assert(gameState === GameState.fromString(gameState.toString))

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

  it should "determine if an attack is valid or not" in {
    val gameState = GameState(
      battleground=Battleground.fromString("1/1 (T), 2/2 vs 3/3"))

    assert(gameState.isValidAttack(Set()))
    assert(gameState.isValidAttack(Set(1)))
    assert(!gameState.isValidAttack(Set(0)))
    assert(!gameState.isValidAttack(Set(0, 1)))

    intercept[IndexOutOfBoundsException] {
      assert(!gameState.isValidAttack(Set(2)))
    }
  }

  it should "declare attackers" in {
    val gameState = GameState(
      battleground = Battleground.fromString("1/1, 2/2 vs 3/3"))

    assert(gameState.declareAttackers(Set(0, 1)) ===
      GameState(
        turnPhase = TurnPhase.DeclareBlockers,
        battleground = Battleground.fromString("1/1 (TA), 2/2 (TA) vs 3/3")))

    assert(gameState.declareAttackers(Set(1)) ===
      GameState(
        turnPhase = TurnPhase.DeclareBlockers,
        battleground = Battleground.fromString("1/1, 2/2 (TA) vs 3/3")))
  }

  it should "perform attack, block and orderblocker moves using apply()" in {
    val gameState =
      GameState.fromString("20/20 (1/DeclareAttackers): 1/1, 2/2 vs 3/3")

    val attackers = Set(0, 1)
    val attackMove = AttackMove(attackers)
    assert(gameState(attackMove) === gameState.declareAttackers(attackers))

    val blockers = Map(0 -> 1)
    val blockMove = BlockMove(blockers)
    assert(gameState(attackMove)(blockMove) ===
      gameState.declareAttackers(attackers).declareBlockers(blockers))

    val gs2 = gameState(attackMove)(blockMove)
    val combatAssignment = gs2.combatAssignment
    val orderBlockersMove = OrderBlockersMove(combatAssignment)
    assert(gs2(orderBlockersMove) === gs2.resolveCombat(combatAssignment))
  }

  it should "determine if a blocking assignment is valid or not" in {
    val gameState = GameState(
      turnPhase = TurnPhase.DeclareBlockers,
      battleground = Battleground.fromString("1/1 (TA), 2/2 vs 1/1 (T), 2/2"))

    assert(gameState.isValidBlock(Map[Int, Int]()))
    assert(gameState.isValidBlock(Map(1 -> 0)))
    assert(!gameState.isValidBlock(Map(1 -> 1)))
    assert(!gameState.isValidBlock(Map(0 -> 0)))
  }

  it should "declare blockers" in {
    val gameState = GameState(
      turnPhase = TurnPhase.DeclareBlockers,
      battleground = Battleground.fromString("1/1 (TA), 2/2 (TA) vs 3/3"))

    assert(gameState.declareBlockers(Map(0 -> 1)) ===
      GameState(
        turnPhase = TurnPhase.CombatStep,
        battleground = Battleground.fromString("1/1 (TA), 2/2 (TA) vs 3/3 (B#1)")))
  }

  it should "compute combat assignments" in {
    val gameState = GameState(
      battleground = Battleground.fromString("1/1 (TA), 2/2 (TA) vs 3/3 (B#0), 4/4 (B#0)"))

    assert(gameState.combatAssignment ===
      Map(0 -> List(0, 1), 1 -> List[Int]()))
  }

  it should "resolve combat damage when player2 is attacked" in {
    val gameState = GameState.fromString(
      "20/20 (1/CombatStep): 2/3 (TA), 4/4 (TA) vs 2/2 (B#1), 3/3 (B#1)")

    val combatAssignment1 = Map(
      0 -> List(),
      1 -> List(0, 1))
    assert(gameState.resolveCombat(combatAssignment1) ===
      GameState.fromString("20/18 (2/DeclareAttackers): 2/3 (T) vs 3/3"))

    val combatAssignment2 = Map(
      0 -> List(),
      1 -> List(1, 0))
    assert(gameState.resolveCombat(combatAssignment2) ===
      GameState.fromString("20/18 (2/DeclareAttackers): 2/3 (T) vs 2/2"))
  }

  it should "resolve combat damage when player1 is attacked" in {
    val gameState = GameState.fromString(
      "20/20 (2/CombatStep):  vs 4/4 (TA), 2/3 (TA)")

    assert(gameState.resolveCombat(gameState.combatAssignment) ===
      GameState.fromString("14/20 (1/DeclareAttackers):  vs 4/4 (T), 2/3 (T)"))
  }

  it should "resolve combat damage - complex scenario #1" in {
    val gameState = GameState.fromString(
      "20/20 (2/CombatStep): 4/4 (B#0) vs 3/3 (TA), 3/3 (TA), 5/5 (TA)")
    assert(gameState.resolveCombat(gameState.combatAssignment) ===
      GameState.fromString("12/20 (1/DeclareAttackers): 4/4 vs 3/3 (T), 5/5 (T)"))
  }

  it should "resolve combat damage - complex scenario #2" in {
    val gameState = GameState.fromString(
      "20/20 (2/CombatStep): 3/3 (B#0), 0/7 (B#0), 4/4 (B#0) vs 10/10 (TA)")
    val combatAssignment = Map(
      0 -> List(1, 2, 0))
    assert(gameState.resolveCombat(combatAssignment) ===
      GameState.fromString("20/20 (1/DeclareAttackers): 3/3, 4/4 vs 10/10 (T)"))
  }

  it should "untap creatures after resolving combat damage" in {
    val gameState = GameState.fromString(
      "20/20 (2/CombatStep): 4/4 (T) vs 3/3 (TA), 5/5")
    assert(gameState.resolveCombat(gameState.combatAssignment) ===
      GameState.fromString("17/20 (1/DeclareAttackers): 4/4 vs 3/3 (T), 5/5"))
  }

  it should "be partially ordered in regards to strictly better" in {
    def assertTryCompareTo(
        s1: String, s2: String, expected: Option[Int]): Unit = {
      val gs1 = GameState.fromString(s1)
      val gs2 = GameState.fromString(s2)
      assert(gs1.tryCompareTo(gs2) === expected, s" for $gs1 and $gs2")
    }

    assertTryCompareTo(
      "20/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      "20/20 (1/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      None)
    assertTryCompareTo(
      "30/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      "20/10 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      None)
    assertTryCompareTo(
      "20/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      "20/10 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      Some(+1))
    assertTryCompareTo(
      "20/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      "20/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 1/1",
      Some(+1))
    assertTryCompareTo(
      "20/20 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 1/1",
      "20/10 (2/DeclareAttackers): 10/10 (T) vs 3/3, 3/3, 3/3, 3/3, 1/1",
      None)

    assertTryCompareTo(
      "20/17 (2/DeclareAttackers): 1/1 (T), 2/2 vs 7/7",
      "20/20 (2/DeclareAttackers): 2/2 vs 7/7",
      Some(-1)
    )
  }

}
