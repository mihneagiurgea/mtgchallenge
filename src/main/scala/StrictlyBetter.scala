package main.scala

/** Holds helpers for the "strictly-better" partial ordering between different
  * game elements.
  */
object StrictlyBetter {

  /* Combines to Option[Int] representing tryCompareTo results into one. */
  def combine(x: Option[Int], y: Option[Int]): Option[Int] = (x, y) match {
    case (None, _) => None
    case (_, None) => None

    // x and y are not comparable
    case (Some(-1), Some(1)) => None
    case (Some(1), Some(-1)) => None

    // x == y
    case (Some(0), Some(0)) => Some(0)

    // x < y
    case _ if (x.get < 0 || y.get < 0) => Some(-1)

    // x > y
    case _ => Some(+1)
  }

}
