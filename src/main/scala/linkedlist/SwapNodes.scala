package linkedlist

object SwapNodes {

  def swapNodes(head: Node, x: Int, y: Int): Node = {
    Option(head) match {
      case Some(h) => head
      case None => head
    }
  }

  def main(args: Array[String]): Unit = {

    // (1, 2, 1, 3) , 2, 3 => (1, 3, 1, 2)
    // (1, 2, 1, 3, 1) , 2, 3 => (1, 3, 1, 2, 1)
    // (2, 1, 1, 3) , 2, 3 => (3, 1, 1, 2)
    // (3, 1, 1, 2) , 2, 3 => (2, 1, 1, 3)
    // (3, 1, 2, 1) , 2, 3 => (2, 1, 3, 1)
    // (2, 1, 3, 1) , 2, 3 => (3, 1, 2, 1)
    // (2, 3, 1) , 2, 3 => (3, 2, 1)
    // (2, 3) , 2, 3 => (3, 2)
    // (3, 2) , 2, 3 => (2, 3)
    // (3, 2) , 2, 3 => (2, 3)
    // null , 2, 3 => null
    // (2, 3, 1) , 4, 3 => (2, 3, 1)
    // (2, 3, 1) , 2, 4 => (2, 3, 1)
    // (1, 2, 3, 1) , 2, 3 => (1, 3, 2, 1)



    println(compareLists(Node.buildFromArray(Array(1)), null))

  }

  def compareLists(a: Node, b: Node): Boolean = {
    Option(a) match {
      case None => Option(b).isEmpty
      case Some(n) => Option(b) match {
        case None => false
        case Some(m) =>
          var i = n
          var j = m
          var dataMatch = i.data == j.data
          while(Option(i.next).isDefined && dataMatch) {
            dataMatch = Option(j).isDefined && i.data == j.data
            i = i.next
            j = j.next
          }
          dataMatch
      }
    }
  }

  def length(n: Node): Int = {
    var l = 0
    while (Option(n.next).isDefined)
      l = l + 1
    l
  }

}
