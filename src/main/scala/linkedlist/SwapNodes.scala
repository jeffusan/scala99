package linkedlist

object SwapNodes {

  def swapNodes(head: Node, x: Int, y: Int): Node = {
    Option(head) match {
      case None => head
      case Some(h) => swapNotNullList(h, x, y)
    }
  }

  private def swapNotNullList(head: Node, x: Int, y: Int): Node = {
    if(contains(head, x) && contains(head, y)) {
      swapExistingElements(head, x, y)
    } else {
      println("lol elements not in list")
      head
    }
  }

  /**
    * assumes:
    * head not null
    * list contains x and y
    */
  private def swapExistingElements(head: Node, x: Int, y: Int): Node = {
    head.data match {
      case a if a == x => swapHead(head, y)
      case b if b == y => swapHead(head, x)
      case _ => head
    }
  }

  /**
    * assumes:
    * head not null
    * list contains x
    */
  private def swapHead(head: Node, x: Int): Node = {

    val y = head.data

    new Node(x, buildAndReplace(head.next, x, y))
  }

  /**
    * replaces the first element found with the x value, with the y value
    */
  private def buildAndReplace(n: Node, x: Int, y: Int): Node = {
    Option(n).map(
      node =>
        if(node.data == x)
          new Node(y, copy(node.next))
        else
          new Node(node.data, buildAndReplace(node.next, x, y))

    ).getOrElse(n)
  }

  private def copy(n: Node): Node = {
    Option(n).map(node => new Node(node.data, copy(node.next)))
      .getOrElse(n)
  }

  def contains(head: Node, x: Int): Boolean = {
    var node = head
    var matched = false
    while(node != null && !matched) {
      matched = node.data == x
      node = node.next
    }
    matched
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
