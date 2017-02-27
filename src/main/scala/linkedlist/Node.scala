package linkedlist

class Node(val data: Int, val next: Node) {

}

object Node {

  def buildFromArray(input: Array[Int]): Option[Node] = {
    Option(input) match {
      case None => None
      case Some(ar) if ar.length == 0 => None
      case Some(a) =>
        Some(a.slice(0, a.length - 1).foldRight(new Node(a(a.length - 1), null))((i,n) => new Node(i, n)))
    }
  }

  def length(head: Node): Int = {
    var l = 0
    var n = head
    while (Option(n).isDefined) {
      l = l + 1
      n = n.next
    }
    l
  }

  def compareLists(a: Node, b: Node): Boolean = {
    Option(a) match {
      case None => Option(b).isEmpty
      case Some(n) => Option(b) match {
        case None => false
        case Some(m) =>
          var i = n
          var j = m
          val l = length(m)
          val k = length(n)
          var dataMatch = l == k && i.data == j.data
          while(Option(i).isDefined && dataMatch) {
            dataMatch = i.data == j.data
            i = i.next
            j = Option(j).map(_.next).orNull
          }
          dataMatch
      }
    }
  }

  def printList(head: Node): String = {
    var n = head
    val b = new StringBuilder
    while(n != null){
      b.append(n.data)
      if(n.next != null)
        b.append(", ")
      n = n.next
    }
//    println(b.toString())
    b.toString()
  }

  def main(args: Array[String]): Unit = {
    printList(buildFromArray(Array(1,2,3)).get)
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
