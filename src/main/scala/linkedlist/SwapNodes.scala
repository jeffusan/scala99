package linkedlist

object SwapNodes {

  /**
    * O(n)
    */
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
      case _ => swapNonHead(head, x, y)
    }
  }

  /**
    * assumes:
    * list not null
    * elements to swap in list
    * head does not contain one of the element to swap
    */
  private def swapNonHead(head: Node, x: Int, y: Int): Node = {
    val xReplacedByY = buildAndReplace(head, x, y)
    buildAndReplaceSecondOccurence(xReplacedByY, y, x)
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
    *
    * note: difficult to use tail recursion because it's a simple linkedlist
    * we need to reach the last element in order to build the second to last
    * and build the new list "backwards"
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

  /**
    * replaces the second element found with the x value, with the y value
    */
  private def buildAndReplaceSecondOccurence(n: Node, x: Int, y: Int): Node = {
    buildAndReplaceSecondOccurence(n, x, y, false)
  }

  /**
    * replaces the second element found with the x value, with the y value
    */
  private def buildAndReplaceSecondOccurence(n: Node, x: Int, y: Int, second: Boolean): Node = {
    Option(n).map(
      node =>
        if(node.data == x && second)
          new Node(y, copy(node.next))
        else if(node.data == x && !second)
          new Node(node.data, buildAndReplaceSecondOccurence(node.next, x, y, true))
        else
          new Node(node.data, buildAndReplaceSecondOccurence(node.next, x, y, second))

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
