package datastructure.tree

import scala.collection.immutable.TreeSet


sealed trait Color
case object Red extends Color
case object Black extends Color

sealed trait Tree[+T] {
  def color: Color
}
case class Node[T](value: T, color: Color, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = value.toString + " " + left.toString + " - " + right.toString
}
case object End extends Tree[Nothing] {
  def color: Color = Black
  override def toString: String = "End"
}


object RedBlack {

  def insert[T <: Ordered[T]](value: T, tree: Tree[T]): Tree[T] = tree match {
      // when we insert a Node, we color it red (this assumes insert will not be called directly on an End).
    case End => Node(value, Red, End, End)
    case n@Node(v,_,l,r) =>
      val root =
        if (value < v) balance(n.copy(left = insert(value, l)))
        else if (v < value) balance(n.copy(right = insert(value, r)))
        else n

      root match {
        case n@Node(_, Red, _, _) => n.copy(color = Black)
        case _ => root
      }
  }

  /**
    * invariants:
    * - If a node is red, then both its children are black
    * - the paths from node to its children contain same number of black nodes
    */
  def balance[T](n: Node[T]): Node[T] = ???

  def main(args: Array[String]): Unit = {

    TreeSet
    println(Node(0, Black, End, End))
  }

}
