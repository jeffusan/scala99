package interviews

object SmallestGroup {

  def smallestGroup(connections: Array[(Int, Int)]): Int = {
    val graph = buildGraph(connections)

    val (m, _) = graph.keys.foldLeft((Int.MaxValue, Set[Int]())) {
      case ((min, visited), i) ⇒
        bfs(graph, visited, i) match {
          case Right((c, v)) ⇒ (Math.min(c, min), v)
          case Left(_) ⇒ (min, visited)
        }
    }
    m
  }

  def buildGraph(connections: Array[(Int, Int)]): Map[Int, List[Int]] = {
    connections.foldLeft(Map[Int, List[Int]]()) {
      case (map, (a, b)) ⇒
        val c0 = map.getOrElse(a, List[Int]())
        val m0 = map.updated(a, b :: c0)
        val c1 = m0.getOrElse(b, List[Int]())
        m0.updated(b, a :: c1)
    }
  }

  def bfs(graph: Map[Int, List[Int]], visited: Set[Int], i: Int): Either[String, (Int, Set[Int])] =  {
    if(visited.contains(i)) Left("visited")
    else {
      val v = visited + i
      val friends = graph.getOrElse(i, Nil)
      Right(friends.foldLeft((1, v)){
        case ((c, vu), e) ⇒
          bfs(graph, vu, e) match {
            case Right((cu, vw)) ⇒ (c + cu, vw)
            case Left(_) ⇒ (c, vu)
          }
      })
    }
  }



}
