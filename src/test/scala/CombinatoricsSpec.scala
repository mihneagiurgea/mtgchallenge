package test.scala

import org.scalatest._
import main.scala.Combinatorics._

class CombinatoricsSpec extends FlatSpec {

  "Combinatorics" should "determine all mappings" in {
    val R = IndexedSeq('a', 'b', 'c')
    val T = IndexedSeq('A', 'B', 'C')

    val mappings = getAllMappings(R, T)
    assert(mappings.length === 27)

    assertNoDuplicates(mappings.toList)
  }

  def assertNoDuplicates[T](items: List[T]): Unit = {
    assert(items.toSet.size === items.length)
  }

}
