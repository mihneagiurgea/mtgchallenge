package main.scala

/** An Enumeration for the defining the outcome of a game.
  */
object Outcome extends Enumeration {
  type Outcome = Value

  val NotOver, Loss, Draw, Win = Value
}