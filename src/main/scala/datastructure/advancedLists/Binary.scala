package datastructure.advancedLists

/**
  * We use a List[Int] to represent binary numbers, a list of 0's and 1's.
  * If you pass in a list that has any other numbers except 0 or 1, the algorithms will throw an exception.
  *
  * We write a binary number from left to right.
  * In other words, the most significant bit is at the leftmost
  * and the least significant bit of a binary number is at the rightmost.
  *
  */
object Binary {

  def carry(c: Int, list: List[Int]): List[Int] = (c, list) match {
    case (0, xs) => xs
    case (1, Nil) => List(1)
    case (1, x::xs) => (1 - x) :: carry(x, xs)
    case (_, _) => sys.error("bad input")
  }

  def add(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (xs, Nil) => xs
    case (Nil, xs) => xs
    case (e::es, f::fs) => ((e + f) % 2) :: carry((e + f) / 2, add(es, fs))
//    case (e::es, f::fs) if e == 0 || f == 0 => (e + f) :: add(es, fs)
//    case (e::es, f::fs) if e == 1 && f == 1 => 0 :: carry(1, add(es, fs))
    case _ => sys.error("invalid input")
  }

  /**
    * assumes bits in reverse order: most significant bit at the tail
    */
  def multiply(a: List[Int], b: List[Int]): List[Int] = b match {
    case Nil => Nil
    case 0 :: xs => 0 :: multiply(a, xs) // assumes there will be a 1 at some point later => left shift operation (multiply by 2)
    case 1 :: xs => add(a, 0 :: multiply(a, xs)) //add itself (a) to final result: the least significant bit will be 0 because the bit is the same
    case _ => sys.error("bad input")
  }

  /**
    * assuming LSB at the head
    */
  def increment(l: List[Int]): List[Int] = l match {
    case Nil => List(1)
    case 0 :: xs => 1 :: xs // 0 + 1 = 1
    case 1 :: xs => 0 :: increment(xs) // 1 + 1 = 0 and carry
    case _ => sys.error("not a binary number")
  }

  def addition(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (xs, Nil) => xs
    case (Nil, xs) => xs
    case (i :: is, 0 :: js) => i :: add(is, js)
    case (0 :: is, j :: js) => j :: add(is, js)
    case (1 :: is, 1 :: js) => 0 :: increment(add(is, js))
    case _ => sys.error("issue")
  }

  def decrement(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case 1 :: Nil => Nil
    case 1 :: xs => 0 :: xs
    case 0 :: xs => 1 :: decrement(xs)
    case _ => sys.error("issue with decrement")

  }

  sealed abstract class Tree {
    def size: Int
  }

  case class Leaf(n: Int) extends Tree {
    override def size: Int = 1
  }

  case object Zero extends Tree {
    override def size: Int = 0
  }

  case class One(t: Tree) extends Tree {
    override def size: Int = t.size
  }

  case class Node(t1: Tree, t2: Tree) extends Tree {
    override def size: Int = t1.size + t2.size
  }

  def link(t1: Tree, t2: Tree): Tree = Node(t1, t2)

  def insert(l: List[Tree], t: Tree): List[Tree] =  l match {
    case Nil => List(One(t))
    case Zero :: tail => One(t) :: tail
    case One(b) :: tail => Zero :: insert(tail, link(t, b)) //note that LSB is the left most one => here the last in dex is the right most (first element inserted is the right most)
    case _ => sys.error("should no end up here while inserting")
  }

  def lift(l: List[Int]): List[Tree] = {
    l.foldRight(List[Tree]()) {
      (i, r) => insert(r, Leaf(i))
    }
  }

  def lookup(l: List[Tree], i: Int): Tree = l match {
    case Nil => Zero
    case Zero :: tail => lookup(tail , i)
    case One(t) :: tail if i < t.size => t
    case One(t) :: tail if i >= t.size => lookup(tail, i - t.size)
    case _ => sys.error("lookup fail")
  }

  def search(tree: Tree, index: Int): Int = (tree, index) match {
    case (Leaf(v), 0) => v
    case (n @ Node(t1, t2), i) if i >= n.size / 2 => search(t2, i - n.size / 2)
    case (n @ Node(t1, t2), i) => search(t1, i)
    case _ => sys.error("search fail")

  }

  def removeTree(t: List[Tree]): (Tree, List[Tree]) = t match {
    case One(x) :: Nil => (x, Nil)
    case One(x) :: ts => (x, Zero :: ts)
    case Zero :: ts =>
      val (Node(t1, t2), rest) = removeTree(ts)
      (t1, One(t2) :: rest)
    case _ => sys.error("error in removeTree")
  }

  def head(l: List[Tree]): Int = removeTree(l) match {
    case (Leaf(x), _) => x
    case _ => sys.error("issue in head")
  }

  def tail(l: List[Tree]): List[Tree] = removeTree(l) match {
    case (_, ts) => ts
    case _ => sys.error("issue in tail")
  }

  /**
    * returns the Tree to update
    */
  def setVal(i: Int, newVal: Int, l: List[Tree]): List[Tree] = l match {
    case Zero :: ts => Zero :: setVal(i, newVal, ts)
    case One(t) :: ts if i < t.size => One(setValInTree(i, newVal, t)) :: ts
    case One(t) :: ts if i >= t.size => One(t) :: setVal(i - t.size, newVal, ts)
    case _ => sys.error("error in setVal")
  }

  def setValInTree(i: Int, newVal: Int, t: Tree): Tree = (i, t) match {
    case (0, Leaf(x)) => Leaf(newVal)
    case (j, n@Node(t1, t2)) if j < n.size / 2 => Node(setValInTree(i, newVal, t1), t2)
    case (j, n@Node(t1, t2)) if j >= n.size / 2 => Node(t1, setValInTree(i - n.size / 2, newVal, t2))
    case _ => sys.error("error in setValInTree")
  }





}
