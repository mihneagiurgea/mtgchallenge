package main.scala

// This import was added because compiler warning said so :)
import scala.language.implicitConversions

/** Holds helpers for the "strictly-better" partial ordering between different
  * game elements.
  */
object StrictlyBetter {

  /* Combines two Option[Int] representing tryCompareTo results into one. */
  def combine(x: Option[Int], y: => Option[Int]): Option[Int] = x match {
    case None => None
    case Some(xValue) => y match {
      case None => None
      case Some(yValue) => combine(xValue, yValue)
    }
  }

  /* Combines two integers representing compareTo results into one. */
  def combine(x: Int, y: Int): Option[Int] = {
    // x and y are not comparable
    if ((x < 0 && y > 0) ||
        (x > 0 && y < 0)) None
    else
      // x == y
      if (x == 0 && y == 0) Some(0)
      else
        // x < y
        if (x < 0 || y < 0) Some(-1)
        // x > y
        else Some(+1)
  }

  // TODO - performance can be improved by specifically designing the
  // maximals & minimals methods to accept sets of PartiallyOrdered
  implicit def setOfPartiallyOrderedToPartiallyOrdered
      [T <% PartiallyOrdered[T]](s: Set[T]): PartiallyOrdered[Set[T]] = {
    new PartiallyOrdered[Set[T]] {

      def tryCompareTo[B >: Set[T]](that: B)
            (implicit arg0: (B) ⇒ PartiallyOrdered[B]) =
        tryCompare(s, that.asInstanceOf[Set[T]])

    }
  }

  /* Try to compare Sets of partially ordered elements.
   *
   * A Set s1 is less than another Set s2 iff all elements from s1 are less
   * than all elements from s2.
   */
  def tryCompare[T <% PartiallyOrdered[T]](s1: Set[T], s2: Set[T]): Option[Int] = {
    def isLessThan(s1: Set[T], s2: Set[T]): Boolean =
      s1.forall(e => s2.forall(e <= _))

    // Small performance optimization
    if (s1.size == 1 && s2.size == 1) s1.head.tryCompareTo(s2.head)
    else
      if (s1 == s2) Some(0)
      else
        if (isLessThan(s1, s2)) Some(-1)
        else
          if (isLessThan(s2, s1)) Some(+1)
          else None
  }

  /* Returns all maximal elements from a list of partially ordered elements. */
  def maximals[T <% PartiallyOrdered[T]](ls: Set[T]): Set[T] = {
    def addToMaximals(maximals: Set[T], e: T): Set[T] = {
      val newMaximals = maximals.filterNot(_ < e)
      if (newMaximals.exists(e < _)) newMaximals
      else newMaximals + e
    }
    ls.foldLeft(Set[T]())(addToMaximals)
  }

  /* Returns all minimal elements from a list of partially ordered elements. */
  def minimals[T <% PartiallyOrdered[T]](ls: Set[T]): Set[T] = {
    def addToMinimals(minimals: Set[T], e: T): Set[T] = {
      val newMinimals = minimals.filterNot(_ > e)
      if (newMinimals.exists(e > _)) newMinimals
      else newMinimals + e
    }
    ls.foldLeft(Set[T]())(addToMinimals)
  }

}
