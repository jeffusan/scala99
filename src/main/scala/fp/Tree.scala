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
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A](f: A => A, tree: Tree[A]): Tree[A] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(f, l), map(f, r))
  }

  def fold[A,B](tree: Tree[A], z:A => B)(f:(B,B) => B): B = tree match {
    case Leaf(v) => z(v)
    case Branch(l,r) => f(fold(l, z)(f),fold(r, z)(f))
  }

  def mapViaFold[A,B](tree: Tree[A], f:A => B): Tree[B] = {
    fold(tree, a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }

}
