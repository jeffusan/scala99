package binarytree

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {

  def main(args: Array[String]) {

//    println(nodeNumberFor(2))
//    println(0 / 2 +1)
//    println(7 / 2)
//    println(0 % 2)
//    println(buildPerfectTree(2, "x", 1))
    println(cBalanced(10, ""))

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

