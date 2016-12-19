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



//    println(compareLists(Node.buildFromArray(Array(1)).get, null))

  }

}
