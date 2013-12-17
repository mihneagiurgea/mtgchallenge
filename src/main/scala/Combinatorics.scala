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

  /** Given a Mapping[T, List[U]], return all mappings m such that:
    *   m.keys() === mapping.keys()
    *   set(m[k]) === set(mapping[k]), forAll k in m
    */
  def getAllShuffledMappings[T, U](
      mapping: Map[T, List[U]]): Seq[Map[T, List[U]]] = {

    def f(keys: List[T]): Seq[Map[T, List[U]]] = keys match {
      case Nil => Seq( Map[T, List[U]]() )
      case key :: tail => {
          val prevs = f(tail)
          val permutationsAtKey = mapping(key).permutations
          val iterator = permutationsAtKey.flatMap(
            permutation => prevs.map(m => m + (key -> permutation)) )
          iterator.toSeq
      }
    }

    f(mapping.keys.toList)
  }

}
