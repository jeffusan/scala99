package linkedlist

class Node(val data: Int, val next: Node) {

}

object Node {
  def buildFromArray(a: Array[Int]): Node = {

    a.slice(0, a.length - 1).foldRight(new Node(a(a.length - 1), null))((i,n) => new Node(i, n))

  }

  def printList(head: Node): Unit = {
    var n = head
    val b = new StringBuilder
    while(n != null){

      b.append(n.data)

      if(n.next != null)
        b.append(", ")

      n = n.next
    }

    println(b.toString())


  }

  def main(args: Array[String]): Unit = {
    printList(buildFromArray(Array(1,2,3)))
  }
}


class IterableNode(val n: Node) extends Iterable[Node] {

  override def iterator: Iterator[Node] = {
    new Iterator[Node]() {
      def hasNext: Boolean = {
        Option(n.next).isDefined
      }

      def next: Node = {
        n.next
      }
    }
  }
}
