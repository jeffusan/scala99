package datastructure.tree

sealed trait BinTree[+A]
case object Leaf extends BinTree[Nothing]
case class Branch[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

object BinTree {
  def buildFromList[A](l: List[A]): BinTree[A] = l match {
    case Nil ⇒ Leaf
    case (x :: xs) ⇒
      val k = xs.length / 2
      Branch(x, buildFromList(xs.take(k)), buildFromList(xs.drop(k)))
  }

  /**
    * The depth of the tree is the length of the longest path from a root to a leaf.
    */

  /**
    * If a binary tree has size(tree) == 2(depth(tree))-1, then it is a complete binary tree.
    */

  type Dictionary[A] = BinTree[(String, A)]

  def insert[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf ⇒ Branch((key, value), Leaf, Leaf)
    case Branch((k,v), l, r) if key < k ⇒ Branch((k, v), insert(key, value, l), r)
    case Branch((k,v), l, r) if key > k ⇒ Branch((k, v), l, insert(key, value, r))
    case Branch((k,v), l, r) if key > k ⇒ sys.error(s"key $key already present")
  }

  def search[A](key: String, dict: Dictionary[A]): Option[A] = dict match {
    case Leaf ⇒ None
    case Branch((k,v), _, _) if key == k ⇒ Some(v)
    case Branch((k,_), l, _) if key < k ⇒ search(key, l)
    case Branch((k,_), _, r) if key > k ⇒ search(key, r)
  }

  def empty[A](): Dictionary[A] = Leaf

  val list = List(("so", 8),
    ("that", 9),
    ("is", 6),
    ("then", 12),
    ("laugh", 4),
    ("for", 12))

  def bstFromList[A](l: List[(String, A)]): Dictionary[A] = l.foldLeft(empty[A]())((t, e) ⇒ insert(e._1, e._2, t))
}


