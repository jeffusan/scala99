package datastructure.queues

object Queue {

  case class Fifo(in: List[Int], out: List[Int]) {
    def check: Boolean = (in, out) match {
      case (x :: xs, Nil) => false
      case _ => true
    }

    require(check, "invariant failed")
  }

}

object PriorityQueue {

  sealed trait TreeNode {
    def rank: Int
  }

  case class Node(rank: Int, v: Int, left: TreeNode, right: TreeNode) extends TreeNode {
    def check: Boolean = left.rank >= right.rank

    require(check)
  }

  case object Leaf extends TreeNode {
    override def rank: Int = 0
  }

  def makeNode(v: Int, left: TreeNode, right: TreeNode): Node = {
    if(left.rank >= right.rank) Node(left.rank + 1, v, left, right)
    else Node(right.rank + 1, v, right, left)
  }

  def merge(a: TreeNode, b: TreeNode): TreeNode = (a, b) match {
    case (Leaf, x) => x
    case (x, Leaf) => x
    case (Node(_, va, la, ra), Node(_, vb, lb, rb)) =>
      if (va < vb ) makeNode(va, la, merge(ra, b))
      else makeNode(vb, lb, merge(rb, a))
  }

  def lift(l: List[Int]): TreeNode = l.foldLeft(Leaf: TreeNode) {
    (t, i) => merge(t, makeNode(i, Leaf, Leaf))
  }

}
