package main.scala

import scala.collection.mutable

object MaximumMatching {

  /** Find a maximum matching in a bipartite graph G = (V = (X U Y), E), where
    * X = 1 to N and Y = 1 to M (for some N and M).
    *
    * @param edges the edges `E` of the graph `G`
    */
  def maximumMatching(edges: Map[Int, List[Int]]): List[Int] = {
    // Implementation uses mutable collections.
    val N = edges.keys.max
    val M = edges.values.flatten match {
      case Nil => 0
      case ls => ls.max
    }

    val L = mutable.IndexedSeq.fill(N + 1)(0)
    val R = mutable.IndexedSeq.fill(M + 1)(0)
    val U = mutable.IndexedSeq.fill(N + 1)(false)

    def pairup(node: Int): Boolean = {
      // Already visited this node.
      if (U(node)) return false
      U(node) = true

      for (adj <- edges(node))
        if (R(adj) == 0) {
          // Add edge (node -> adj) to current matching.
          L(node) = adj
          R(adj) = node
          return true
        }
      for (adj <- edges(node))
        if (pairup(R(adj))) {
          // Replace edge (R(adj) -> adj) with edge (node -> adj)
          L(node) = adj
          R(adj) = node
          return true
        }
      return false
    }

    var changed = false
    do {
      changed = false
      for (i <- 1 to N) U(i) = false
      for (i <- 1 to N)
        if (L(i) == 0)
          changed |= pairup(i)
    } while (changed)

    L.toList.tail
  }

  /** Returns true if the given bipartite graph G = (V = (X U Y), E), where
    * X = 1 to N and Y = 1 to M (for some N and M) has a perfect matching that
    * matches all vertices from X.
    *
    * @param edges the edges `E` of the graph `G`
    */
  def hasPerfectMatching(edges: Map[Int, List[Int]]): Boolean = {
    val matching = maximumMatching(edges)
    matching.count(_ > 0) == edges.size
  }

}
