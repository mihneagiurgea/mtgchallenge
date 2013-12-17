package test.scala

import org.scalatest._
import main.scala.Combinatorics._

class CombinatoricsSpec extends FlatSpec {

  "Combinatorics" should "determine all mappings" in {
    val R = IndexedSeq('a', 'b', 'c')
    val T = IndexedSeq('A', 'B', 'C')

    val mappings = getAllMappings(R, T)
    assert(mappings.length === 27)

    assertNoDuplicates(mappings)
  }

  it should "determine all shuffled mappings" in {
    val mapping = Map(
      'a' -> List(4),
      'b' -> List(5, 6),
      'c' -> List(7, 8, 9),
      'd' -> List[Int]())

    val shuffledMappings = getAllShuffledMappings(mapping)
    assert(shuffledMappings.length === 12)

    assert(shuffledMappings.forall(m => m.keys == mapping.keys))

    assert(shuffledMappings.forall(
      m => m.keys.forall(key => m(key).toSet == mapping(key).toSet)))

    assertNoDuplicates(shuffledMappings)
  }

  def assertNoDuplicates[T](items: Seq[T]): Unit = {
    assert(items.toSet.size === items.length)
  }

}
