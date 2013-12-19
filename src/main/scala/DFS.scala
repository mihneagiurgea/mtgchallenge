package main.scala

object DFS {

  def DFS[T](start: T, neighbours: (T) => List[T]): List[T] = {
    def DFS0(node: T, visited: Set[T], ordering: List[T]): (Set[T], List[T]) = {
      if (visited.contains(node))
        (visited, ordering)
      else {
        val adjacentNotVisited = neighbours(node).filterNot(visited.contains)
        val z = (visited + node, node :: ordering)
        adjacentNotVisited.foldLeft(z)(
          (res, e) => DFS0(e, res._1, res._2)
        )
      }
    }

    DFS0(start, Set[T](), List[T]())._2.reverse
  }

  // TODO - distinguish between Forward edges and Cross edges
  case class DFSTreeFunction[T, A](
    z: (T) => A,
    opTreeEdge: (A, A) => A,
    opForwardOrCrossEdge: (A, A) => A,
    opBackEdge: (A) => A) {
  }

  def DFSFold[T, A](
      start: T, neighbours: (T) => List[T])(f: DFSTreeFunction[T, A]): A = {

    type ProgressType = (Set[T], Map[T, A], A)

    def op(s: ProgressType, node: T): ProgressType = {
      val (visited, nodeToResult, partialResult) = s
      if (visited contains node) {
        val result =
          if (nodeToResult contains node)
            // ForwardEdge or CrossEdge
            f.opForwardOrCrossEdge(partialResult, nodeToResult(node))
          else
            // BackEdge
            f.opBackEdge(partialResult)
        (visited, nodeToResult, result)
      } else {
        // TreeEdge
        val nodeResult = DFS(node, visited, nodeToResult)
        (nodeResult._1, nodeResult._2,
          f.opTreeEdge(partialResult, nodeResult._3))
      }
    }

    def DFS(node: T, visited: Set[T], nodeToResult: Map[T, A]): ProgressType = {
      val init = (visited + node, nodeToResult, f.z(node))
      val (newVisited, newNodeToResult, result) =
        neighbours(node).foldLeft(init)(op)
      (newVisited, newNodeToResult + (node -> result), result)
    }

    DFS(start, Set[T](), Map[T, A]())._3
  }

}