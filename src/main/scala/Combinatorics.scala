package main.scala

import math.pow

object Combinatorics {

  /** Returns all mappings f: domain -> codomain such that f is an arbitrary
    * function (not necessary injective).
    */
  def getAllMappings[T](
      domain: IndexedSeq[T], codomain: IndexedSeq[T]): Seq[Map[T, T]] = {
    val N = domain.length
    val M = codomain.length

    def toBase(n: Int, base: Int, digits: Int): List[Int] = digits match {
      case 0 => Nil
      case _ => (n % base) :: toBase(n / base, base, digits-1)
    }

    def makeMap(i: Int): Map[T, T] = {
      // Interpret i as an N-digit number in base M.
      val digits = toBase(i, M, N)
      val functionImage = digits.map(codomain(_))
      domain.zip(functionImage).toMap
    }

    val nrMappings = pow(M, N).toInt
    (0 until nrMappings).map(makeMap(_))
  }

}
