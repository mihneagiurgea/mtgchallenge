package main.scala

/** An Enumeration for the defining the outcome of a game.
  */
object Outcome extends Enumeration {
  type Outcome = Value

  def max(o1: Outcome, o2: Outcome): Outcome = Outcome(o1.id max o2.id)

  def reverse(outcome: Outcome): Outcome = outcome match {
    case Outcome.Loss => Outcome.Win
    case Outcome.Win => Outcome.Loss
    case Outcome.Draw => Outcome.Draw
  }

  val NotOver, Loss, Draw, Win = Value
}
