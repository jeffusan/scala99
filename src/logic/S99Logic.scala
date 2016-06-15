package logic

import language.implicitConversions
import scala.collection.mutable

class S99Logic(a: Boolean) {

  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }

  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _         => false
  }

}

object S99Logic {

  implicit def bool2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

  def main(args: Array[String]) {
//    println(table2(impl))
//    println(gray(1))
//    println(gray(2))
//    println(gray(3))
//    println(gray(4))

    println(huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))

  }

  def not(a: Boolean) = a match {
    case true  => false
    case false => true
  }

  def equ(a: Boolean, b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(a or b)
  def nand(a: Boolean, b: Boolean): Boolean = not(a and b)
  def impl(a: Boolean, b: Boolean): Boolean = not(a) or b

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false)
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

  private var memoizedGrays: Map[Int, List[String]] = Map()

  def gray(n: Int): List[String] = {

    memoizedGrays.get(n) match {
      case Some(g) => g
      case None =>
        if(n == 0) List("")
        else {
          val lower = gray(n-1)
          val g = lower.map("0" + _) ::: lower.reverse.map("1" + _)
          memoizedGrays += (n -> g)
          g
        }
    }
  }

  def huffman(list: List[(String,Int)]): List[(String,String)] = {

    //define a node
    //implement a priority queue based on Heap for Log(n)
    //create a priority queue of nodes based on the lowest freq first
    // create huffman tree
    // go through the tree in breadth first manner

    val minPriorityQueue: mutable.PriorityQueue[HuffmanNode] = mutable.PriorityQueue.empty(MinFreqOrder)
    list.foreach {
      c: (String,Int) => minPriorityQueue.enqueue(new HuffmanNode(c._1, c._2, null, null))
    }

    while(minPriorityQueue.length >= 2) {
      val node1 = minPriorityQueue.dequeue()
      val node2 = minPriorityQueue.dequeue()

      val node3 = new HuffmanNode(null, node1.freq + node2.freq, node1, node2)
      minPriorityQueue.enqueue(node3)
    }

    val root = minPriorityQueue.dequeue()


    val result: List[(String,String)] = List()

    traverse(root, result, "")
  }

  def traverse(node: HuffmanNode, result: List[(String, String)], branch: String): List[(String, String)] = {
    if(node.left == null) {
      (node.value, branch) :: result
    } else {
      val leftResult = traverse(node.left, result, branch + "0")
      val rightResult = traverse(node.right, result, branch + "1")
      leftResult ::: rightResult
    }
  }

  object MinFreqOrder extends Ordering[HuffmanNode] {
    def compare(x:HuffmanNode, y:HuffmanNode) = y.freq compare x.freq
  }

 case class HuffmanNode(value: String, freq: Int, left: HuffmanNode, right: HuffmanNode)



  object MinOrder extends Ordering[Int] {
     def compare(x:Int, y:Int) = y compare x
  }

}
