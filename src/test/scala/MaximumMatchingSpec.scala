package test.scala

import org.scalatest.FlatSpec

import main.scala.MaximumMatching

class MaximumMatchingSpec extends FlatSpec {

  "MaximumMatching" should "find a maximum matching for a given graph #1" in {
    val edges = Map(
      1 -> List(1, 2),
      2 -> List(1, 3),
      3 -> List(3))
    val matching = MaximumMatching.maximumMatching(edges)

    assert(matching.length === edges.size)
    assertIsMatchingOfSize(matching, 3)
    assert(matching === List(2, 1, 3))
  }

  it should "find a maximum matching for a given graph #2" in {
    val edges = Map(
      1 -> List(1, 3),
      2 -> List(1, 2, 4),
      3 -> List(2),
      4 -> List(2))
    val matching = MaximumMatching.maximumMatching(edges)

    assert(matching.length === edges.size)
    assertIsMatchingOfSize(matching, 3)
  }

  it should "find a maximum matching for a degenerate graph" in {
    val edges = Map(
      1 -> List[Int](),
      2 -> List[Int](),
      3 -> List[Int]())

    val matching = MaximumMatching.maximumMatching(edges)
    assert(matching.length === edges.size)
    assertIsMatchingOfSize(matching, 0)
  }

  it should "know if a perfect matching exists" in {
    val edges = Map(
      1 -> List(1, 3),
      2 -> List(1, 2, 4),
      3 -> List(2))

    assert(MaximumMatching.hasPerfectMatching(edges))
    assert(!MaximumMatching.hasPerfectMatching(edges + (4 -> List(2))))
  }

  def assertIsMatchingOfSize(matching: List[Int], size: Int): Unit = {
    val matchedVertices = matching.filterNot(_ == 0)
    assert(matchedVertices.length === size)
    assert(matchedVertices.distinct.length === size)
  }

}
