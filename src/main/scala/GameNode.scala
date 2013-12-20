package main.scala

import Outcome._

trait GameNode {

  def nextToAct: Int

  def isLeaf: Boolean

  def outcome: Outcome

}

trait GameGraph[T <: GameNode] {

  def getNextStates(gameNode: T): Iterator[T]

}
