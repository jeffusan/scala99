package datastructure.queues

object Queue {

  case class Fifo(in: List[Int], out: List[Int]) {
    def check: Boolean = (in, out) match {
      case (x :: xs, Nil) => false
      case _ => true
    }

    require(check, "invariant failed")
  }

  def push(x: Int, q: Fifo): Fifo = q.out match {
    case Nil => q.copy(in = Nil, out = (x :: q.in).reverse)
    case _ => q.copy(in = x :: q.in)
  }

  def pop(q: Fifo): (Int, Fifo) = q.out match {
    case Nil => throw new IllegalArgumentException
    case x :: Nil => (x, q.copy(Nil, q.in.reverse))
    case x :: xs => (x, q.copy(out = xs))
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

  def insert(v: Int, h: TreeNode): TreeNode = {
    val n = Node(1, v, Leaf, Leaf)
    merge(n, h)
  }

  def min(t: TreeNode): Int = t match {
    case Leaf => throw new IllegalArgumentException("tree is empty")
    case Node(_, v, _, _) => v
  }

  def pop(t: TreeNode): (Int, TreeNode) = t match {
    case Leaf => throw new IllegalArgumentException("tree is empty")
    case Node(_, v, a, b) => (v, merge(a, b))
  }

  def test(): Unit = {
    val q = List.range(1, 1000000).view.reverse
  }

  case class LazyQueue(in: List[Int], inLen: Int, out: Stream[Int], outLen: Int) {
    def push(e: Int): LazyQueue = {
      makeLazyQueue(e :: in, inLen + 1, out, outLen)
    }

    def pop(): (Int, LazyQueue) = {
      (out.head, makeLazyQueue(in, inLen, out.tail, outLen - 1))
    }
  }

  def makeLazyQueue(in: List[Int], inLen: Int, out: Stream[Int], outLen: Int): LazyQueue = {
    if(inLen <= outLen) LazyQueue(in, inLen, out, outLen)
    else {
      LazyQueue(List[Int](), 0, copyInToOut(in, out, Stream.empty), outLen + inLen)
    }
  }

  /**
    * Because the streams do computation on demand, it will not actually recursively call itself immediately,
    * but instead "store" how to get to next element.
    */
  def copyInToOut(in: List[Int], out: Stream[Int], revIn: Stream[Int]): Stream[Int] = in match {
    case Nil => Stream.empty
    case x :: xs if out.isEmpty =>
      require(in.length == 1) // for tests only. this degrades perfs
      /**
        * the only case where that can happen is when 'in' has only 1 element:
        * the invariant is that 'in.lenght' is less or equal to 'out.lenght'
        * so if 'out' is empty, then 'in' must be of size 1.
        */
      Stream.cons(x, revIn)
    case x :: xs => Stream.cons(out.head, copyInToOut(in.tail, out.tail, Stream.cons(x, revIn)))
  }



}

case class Deque[T](outLen: Int, out: Stream[T], inLen: Int, in: Stream[T], c: Int = 2) {
  def pushFront(i: T): Deque[T] = {
    rebalance(outLen + 1, Stream.cons(i, out), inLen, in)
  }

  def popFront(): (T, Deque[T]) = out match {
    case Stream.Empty => throw new Exception("poping from enpty deque")
    case e #:: tail => (e, rebalance(outLen - 1, tail, inLen, in))
  }

  def rebalance(outLen: Int, out: Stream[T], inLen: Int, in: Stream[T]): Deque[T] = {
    if (outLen - inLen >= c) {
      //take c from out and put it in in.
      val diff = outLen - c
      val newOut = out.take(outLen - diff)
      val newIn = in.append(out.drop(outLen - diff).reverse)
      Deque(outLen - diff, newOut, inLen + diff, newIn)
    }
    else if (inLen - outLen >= c){
      //take c from in and put it in out.
      val diff = inLen - c
      val newIn = in.take(inLen - diff)
      val newOut = out.append(in.drop(inLen - diff).reverse)
      Deque(outLen + diff, newOut, inLen - diff, newIn)
    }
    else
      Deque(outLen, out, inLen, in, c)
  }
}
