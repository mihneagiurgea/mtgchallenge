package test.scala

import org.scalatest._
import main.scala.DFS._

class DFSSpec extends FlatSpec {

  val neighbours = Map(
    1 -> List(2, 3, 4),
    2 -> List(3),
    3 -> List[Int](),
    4 -> List(1, 3, 5),
    5 -> List(3, 4, 1, 2))
  /* The DFS tree rooted at 1 should look like this:
    1 -> 2 -> 3
      -> 3 (visited)
      -> 4 -> 1, 3 (visited)
           -> 5 -> 3, 4, 1, 2 (visited)
    */

  "DFS" should "traverse all nodes in order" in {
    assert(DFS(1, neighbours) === List(1, 2, 3, 4, 5))
    assert(DFS(2, neighbours) === List(2, 3))
    assert(DFS(3, neighbours) === List(3))
    assert(DFS(4, neighbours) === List(4, 1, 2, 3, 5))
    assert(DFS(5, neighbours) === List(5, 3, 4, 1, 2))
  }

  it should "compute concatenating children on a dfs tree" in {
    val f = DFSTreeFunction[Int, String](
      z = (i: Int) => s"$i",
      opTreeEdge = (i: String, j: String) => i + j,
      opForwardOrCrossEdge = (i: String, j: String) => i,
      opBackEdge = (i: String) => i)

    assert(DFSFold(1, neighbours)(f) === "12345")
  }

  it should "compute concatenating children on a dfs tree and mark back edges" in {
    val f = DFSTreeFunction[Int, String](
      z = (i: Int) => s"$i",
      opTreeEdge = (i: String, j: String) => i + j,
      opForwardOrCrossEdge = (i: String, j: String) => i,
      opBackEdge = (i: String) => i + ".")

    assert(DFSFold(1, neighbours)(f) === "1234.5..")
  }

  it should "concatenate strings on a dfs tree by considering all edge types" in {
    val f = DFSTreeFunction[Int, String](
      z = (i: Int) => s"$i",
      opTreeEdge = (i: String, j: String) => i + j,
      opForwardOrCrossEdge = (i: String, j: String) => s"$i-${j.length}-",
      opBackEdge = (i: String) => i + ".")

    assert(DFSFold(2, neighbours)(f) === "23")
    assert(DFSFold(1, neighbours)(f) === "123-1-4.-1-5-1-..-2-")
  }

}
