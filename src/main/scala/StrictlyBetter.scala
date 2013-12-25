package main.scala

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

}
