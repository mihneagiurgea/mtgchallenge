package main.scala

/** An Enumeration of all turn phases and stes:
  *  - DeclareAttackers: The initial state, before attackers have been declared.
  *  - DeclareBlockers: Before blockers have been declared, but after attackers.
  *  - CombatStep: CombatStep step, in which combat damage is dealt.
  *  - EndOfTurn: After the combat Phase, we'll skip directly to End of turn.
  */
object TurnPhase extends Enumeration {
  type TurnPhase = Value

  val DeclareAttackers, DeclareBlockers, CombatStep, EndOfTurn = Value
}