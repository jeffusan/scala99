package binarytree

sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def addValue[U](x: Any): Tree[Any]

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  def isSymmetric: Boolean = left.isMirrorOf(right)

  def isMirrorOf[V](tree: Tree[V]): Boolean = {
    tree match {
      case node : Node[V] =>  left.isMirrorOf(node.right) && right.isMirrorOf(node.left)
      case _ => false
    }
  }

  def addValue[U](x: Any): Tree[Any] = {

    // assumption: nodes are added on the left first: if a tree is not symmetric, then the left node has more nodes
    if (isSymmetric) {
      left match {
        case node: Node[U] => Node(value, node.addValue(x), right)
        case _ => Node(value, Node(x, End, End), End)
      }
    } else {
      right match {
        case node: Node[U] => Node(value, left, right.addValue(x))
        case _ => Node(value, left, Node(x, End, End))
      }
    }
    // decide where to add, right most first
    // recursively copy current starting from bottom and new node
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  def isSymmetric: Boolean = true

  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case End => true
    case _ => false
  }

  def addValue[U](x: Any): Tree[Any] = Node(x, End, End)

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {

  def main(args: Array[String]) {

//    val tree = cBalanced(3, "")
//    println(tree)
//    println(tree.head.isSymmetric)
//
//    val tree2 = cBalanced(4, "")
//    println(tree2)
//    println(tree2.head.isSymmetric)

//    val a = End.addValue(2)
//    println(a)
//    val b = a.addValue(3)
//    println(b)
//    val c = b.addValue(9)
//    println(c)
//    val d = c.addValue(4)
//    println(d)

//    println(Tree.fromList(List(3, 2, 5, 7, 1)))

//    heightBalancedTrees(4, "").foreach(println(_))

//    println(minHbalNodes(5))

    println(3/2)

    (2 to 16).foreach(i => println(i + ": " + maxHeightFromNodes(i)))

//    val value = "test"
//    println("%s=%.2s%s" format("password", value, value.substring(value.length).padTo(value.length - 1, "*").mkString))
  }

  def maxHeightFromNodes(nodes: Int): Int = nodes match {
    case n if n < 1 => 0
    case 1 => 1
    case 2 => 2
    case n => 1 + maxHeightFromNodes(n/2)
  }

  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1 => 1
    case 2 => 2
    case _ => 1 + minHbalNodes(height - 1) + minHbalNodes(height - 2)
  }

  def heightBalancedTrees[T](i: Int, value: T): List[Tree[T]] = i match {
    case n if n < 1 => List(End)
    case  1 => List(Node(value, End, End))
    case _ =>
      val high = heightBalancedTrees(i - 1, value)
      val low = heightBalancedTrees(i -2, value)
      high.flatMap(l => high.map( r => Node(value, l, r))) :::
        high.flatMap(h => low.flatMap(l => List(Node(value, h, l), Node(value, l, h))))
  }

  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1          => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees(height - 1, value)
      val short = hbalTrees(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
        fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

  def fromList[T](list: List[T]): Tree[Any] = {
    list.foldLeft(End: Tree[Any])((node, value) => node.addValue(value))
  }

  def cBalanced(n: Int, value: String): List[Node[String]] = {

    // count levels
    // build "perfect" tree
    // count nodes left after last complete level of tree
    // build "left" node(s) and add them to each leaves of the perfect tree

    var levelCounter = 0
    while(n >= nodeNumberFor(levelCounter)) levelCounter += 1

    val levels = levelCounter - 1
    val remainingNodes = n - nodeNumberFor(levels)

    List(buildPerfectTree(levels, value, remainingNodes))

  }

  def buildPerfectTree[T](level: Int, value: T, remaining: Int): Node[T] =
  {
    if(level == 0) {

      if(remaining == 2)
        Node(value, Node(value, End, End), Node(value, End, End))
      else if (remaining == 1)
        Node(value, Node(value, End, End), End)
      else
        Node(value, End, End)

    } else {

      if (remaining == 0)
        Node(value, buildPerfectTree(level - 1, value, 0), buildPerfectTree(level - 1, value, 0))
      else if (remaining % 2 == 0)
        Node(value, buildPerfectTree(level - 1, value, remaining / 2), buildPerfectTree(level - 1, value, remaining / 2))
      else
        Node(value, buildPerfectTree(level - 1, value, remaining / 2 + 1), buildPerfectTree(level - 1, value, remaining / 2))

    }
  }

  /** level zero based */
  def nodeNumberFor(level: Int): Int = {

    if(level == 0) 1
    else {
      nodeNumberAt(level) + nodeNumberFor(level - 1)
    }
  }

  def nodeNumberAt(level: Int): Int = Math.pow(2.toDouble, level.toDouble).toInt
}

