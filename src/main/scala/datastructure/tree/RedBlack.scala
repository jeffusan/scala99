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
  def balance[T](n: Node[T]): Node[T] = n match {
    case n@Node(Black, cl@Node(Red, Node(Red, End, _, End), _, End), _, End) => cl.copy(right = n)
  }

  def main(args: Array[String]): Unit = {

    println(Node(Black, End, 0, End))
  }

}
