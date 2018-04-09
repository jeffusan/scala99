package datastructure.tree

sealed trait Color
case object Red extends Color
case object Black extends Color

sealed trait Tree[+T] {
  def color: Color
}
case class Node[T](color: Color, left: Tree[T], value: T, right: Tree[T]) extends Tree[T] {
  override def toString: String = value.toString + " " + left.toString + " - " + right.toString
}
case object End extends Tree[Nothing] {
  def color: Color = Black
  override def toString: String = "End"
}


object RedBlack {

  def insert[T <: Ordered[T]](value: T, tree: Tree[T]): Tree[T] = {

    def ins(t: Tree[T]): Tree[T] = t match {
      case End => Node(Red, End, value, End)
      case n@Node(_,l,v,r) =>
        if (value < v) balance(n.copy(left = ins(l)))
        else if (v < value) balance(n.copy(right = ins(r)))
        else n
    }

    val root = ins(tree)

    root match {
      case n@Node(Red,_, _, _) => n.copy(color = Black)
      case _ => root
    }
  }

  /**
    * invariants:
    * - If a node is red, then both its children are black
    * - the paths from node to its children contain same number of black nodes
    */
  def balance[T](node: Node[T]): Node[T] = node match {
    case n@Node(Black, cl@Node(Red, gcl@Node(Red, End, _, _), _, gcr), _, cr) => cl.copy(left = gcl.copy(color = Black), right = n.copy(left = gcr, right = cr))
    case n@Node(Black, cl@Node(Red, gcl,_, gcr@Node(Red,_,_,_)), _,cr) => ???
    case n@Node(Black, cl, _,cr@Node(Red, gcl, _, gcr@Node(Red, _,_,_))) => ???
    case n@Node(Black, cl, _,cr@Node(Red, gcl@Node(Red, _, _, _), _, gcr)) => ???
    case _ => node
  }

  def main(args: Array[String]): Unit = {

    println(Node(Black, End, 0, End))
  }

}
