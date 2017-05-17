package fp

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    def computeSize(tree: Tree[A], c: Int): Int = tree match {
      case l: Leaf[A] => c + 1
      case b: Branch[A] => 1 + computeSize(b.left, c) + computeSize(b.right, c)
    }
    computeSize(tree, 0)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case l: Leaf[Int] => l.value
    case b: Branch[Int] => max(b.left) max max(b.right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case l: Leaf[Int] => 1
    case b: Branch[Int] => (1 + depth(b.left)) max (1 + depth(b.right))
  }
}
