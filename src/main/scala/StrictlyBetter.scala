package main.scala

/** Holds helpers for the "strictly-better" partial ordering between different
  * game elements.
  */
object StrictlyBetter {

  /* Combines two Option[Int] representing tryCompareTo results into one. */
  def combine(x: Option[Int], y: Option[Int]): Option[Int] = (x, y) match {
    case (None, _) => None
    case (_, None) => None

    case (Some(xValue), Some(yValue)) => combine(xValue, yValue)
  }

  /* Combines two integers representing compareTo results into one. */
  def combine(x: Int, y: Int): Option[Int] = (x, y) match {
    // x and y are not comparable
    case (-1, 1) => None
    case (1, -1) => None

    // x == y
    case (0, 0) => Some(0)

    // x < y
    case _ if (x < 0 || y < 0) => Some(-1)

    // x > y
    case _ => Some(+1)
  }

}
