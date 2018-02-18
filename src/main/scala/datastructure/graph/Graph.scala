package datastructure.graph

object Graph {

  def succSet(a: String, g: List[(String, String)]): List[String] = g match {
    case Nil => Nil
    case x :: xs if x._1 == a => x._2 :: succSet(a, xs)
    case _ :: xs => succSet(a, xs)
  }

  /**
    * assumes directed graph, with fist element of the list being the "entry point" of the graph
    */
  def depthFirst(graph: List[(String, String)]): List[String] = {

    def depthf(suc: List[String], v: List[String]): List[String] = suc match {
      case Nil => v
      case x :: xs if v.contains(x) => depthf(xs, v)
      case x :: xs => depthf(succSet(x, graph) ++ xs, x :: v)
    }

    depthf(List(graph.head._1), List()).reverse

  }

  def depthFirst1(graph: List[(String, String)]): List[String] = {

    def depthf(suc: List[String], v: List[String]): List[String] = suc match {
      case Nil => v
      case x :: xs => depthf(xs, if (v.contains(x)) v else depthf(succSet(x, graph), x :: v))
    }

    depthf(graph.unzip._1, List()).reverse
  }

  def topsort(g: List[(String, String)]): List[String] = {
    def sort(l: List[String], v: List[String]): List[String] = l match {
      case Nil => v
      case x :: xs => sort(xs, if(v.contains(x)) v else x :: sort(succSet(x, g), v))
    }
    sort(g.unzip._1, List())
  }

  case class Cycle(e: String)

  def topsortOrCycle(g: List[(String, String)]): Either[Cycle, List[String]] = {
    def sort(l: List[String], v: List[String], path: List[String]): Either[Cycle, List[String]] = l match {
      case Nil => Right(v)
      case x :: xs =>
        if(v.contains(x)) sort(xs, v, path)
        else if (path.contains(x)) Left(Cycle(x))
        else
          sort(succSet(x, g), v, x :: path) match {
            case l @ Left(_) => l
            case Right(r) => Right(x :: r)
          }

    }
    sort(g.unzip._1, List(), List())
  }

}
