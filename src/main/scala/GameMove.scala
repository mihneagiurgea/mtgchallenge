package main.scala

trait GameMove {

}

case class AttackMove(attackingCreatureUids: Set[Int]) extends GameMove {

}

case class BlockMove(blockingAssignment: Map[Int, Int]) extends GameMove {

}

case class OrderBlockersMove(combatAssignment: Map[Int, List[Int]])
    extends GameMove {

}
