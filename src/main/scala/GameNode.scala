package main.scala

import Outcome._

trait GameNode {

  def nextToAct: Int

  def isLeaf: Boolean

  def outcome: Outcome

}

trait GameGraph[T <: GameNode] {

  def successor(gameNode: T): Set[T] = getNextStates(gameNode)

  def getNextStates(gameNode: T): Set[T]

}
