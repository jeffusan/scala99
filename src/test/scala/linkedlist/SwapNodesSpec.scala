package linkedlist

import org.scalactic.{Equality, Prettifier}
import org.scalatest.{FlatSpec, Matchers}

class SwapNodesSpec extends FlatSpec with Matchers {


  implicit val equality = new Equality[Node] {
    override def areEqual(a: Node, b: Any): Boolean = (a, b) match {
      case (m: Node, n: Node) => Node.compareLists(m,n)
      case _ => false
    }
  }

  implicit val prettifier = new Prettifier {
    def apply(o: Any): String = o match {
      case n: Node => Node.printList(n)
    }
  }

  "equality comparator" should "succeed" in {
    val input = Node.buildFromArray(Array(1,2,3)).get
    val expected = Node.buildFromArray(Array(1,2,3)).get

    input shouldEqual expected
  }

  // (1, 2, 1, 3) , 2, 3 => (1, 3, 1, 2)
  "Swapping middle and last" should "" in {
    val input = Node.buildFromArray(Array(1,2,1,3)).get
    val expected = Node.buildFromArray(Array(1,3,1,2)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (1, 2, 1, 3, 1) , 2, 3 => (1, 3, 1, 2, 1)
  "Swapping both in middle " should "" in {
    val input = Node.buildFromArray(Array(1, 2, 1, 3, 1)).get
    val expected = Node.buildFromArray(Array(1, 3, 1, 2, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (2, 1, 1, 3) , 2, 3 => (3, 1, 1, 2)
  "Swapping first and last " should "" in {
    val input = Node.buildFromArray(Array(2, 1, 1, 3)).get
    val expected = Node.buildFromArray(Array(3, 1, 1, 2)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (3, 1, 1, 2) , 2, 3 => (2, 1, 1, 3)
  "Swapping first and last 2" should "" in {
    val input = Node.buildFromArray(Array(3, 1, 1, 2)).get
    val expected = Node.buildFromArray(Array(2, 1, 1, 3)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (3, 1, 2, 1) , 2, 3 => (2, 1, 3, 1)
  "Swapping first and middle" should "" in {
    val input = Node.buildFromArray(Array(3, 1, 2, 1)).get
    val expected = Node.buildFromArray(Array(2, 1, 3, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (2, 1, 3, 1) , 2, 3 => (3, 1, 2, 1)
  "Swapping first and middle 2" should "" in {
    val input = Node.buildFromArray(Array(2, 1, 3, 1)).get
    val expected = Node.buildFromArray(Array(3, 1, 2, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (2, 3, 1) , 2, 3 => (3, 2, 1)
  "Swapping first and second out of 3" should "" in {
    val input = Node.buildFromArray(Array(2, 3, 1)).get
    val expected = Node.buildFromArray(Array(3, 2, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (3, 2, 1) , 2, 3 => (2, 3, 1)
  "Swapping first and second out of 3 v2" should "" in {
    val input = Node.buildFromArray(Array(3, 2, 1)).get
    val expected = Node.buildFromArray(Array(2, 3, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (2, 3) , 2, 3 => (3, 2)
  "Swapping first and second out of 2" should "" in {
    val input = Node.buildFromArray(Array(2, 3)).get
    val expected = Node.buildFromArray(Array(3, 2)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (3, 2) , 2, 3 => (2, 3)
  "Swapping first and second out of 2 v2" should "" in {
    val input = Node.buildFromArray(Array(3, 2)).get
    val expected = Node.buildFromArray(Array(2, 3)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // null , 2, 3 => null
  "Swapping a null list" should "return a null" in {

    val expected = null

    val result = SwapNodes.swapNodes(null, 2, 3)

    result shouldBe expected
  }

  // (2, 3, 1) , 4, 3 => (2, 3, 1)
  "Swapping with first element not present" should "not swap" in {
    val input = Node.buildFromArray(Array(2, 3, 1)).get

    val result = SwapNodes.swapNodes(input, 4, 3)

    result shouldEqual input
  }

  // (2, 3, 1) , 2, 4 => (2, 3, 1)
  "Swapping with 2nd element not present" should "not swap" in {
    val input = Node.buildFromArray(Array(2, 3, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 4)

    result shouldEqual input
  }

  // (1, 2, 3, 1) , 2, 3 => (1, 3, 2, 1)
  "Swapping adjacent elements in not at the limit" should "" in {
    val input = Node.buildFromArray(Array(1, 2, 3, 1)).get
    val expected = Node.buildFromArray(Array(1, 3, 2, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (1) 2,3 => (1)
  "Swapping on a list of length 1" should "return the list" in {
    val input = Node.buildFromArray(Array(1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual input
  }

  // (1, 2, 1, 2, 3, 1) , 2, 3 => (1, 3, 1, 2, 2, 1)
  "Swapping elements when first appears multiple times " should
    "swap the first found element" in {
    val input = Node.buildFromArray(Array(1, 2, 1, 2, 3, 1)).get
    val expected = Node.buildFromArray(Array(1, 3, 1, 2, 2, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

  // (1, 2, 1, 3, 3, 1) , 2, 3 => (1, 3, 1, 2, 3, 1)
  "Swapping elements when second appears multiple times " should
    "swap the first found element" in {
    val input = Node.buildFromArray(Array(1, 2, 1, 3, 3, 1)).get
    val expected = Node.buildFromArray(Array(1, 3, 1, 2, 3, 1)).get

    val result = SwapNodes.swapNodes(input, 2, 3)

    result shouldEqual expected
  }

}
