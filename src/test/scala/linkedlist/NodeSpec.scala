package linkedlist

import org.scalatest._

import scala.collection.mutable.Stack

class NodeSpec extends FlatSpec with Matchers {

//  "A Stack" should "pop values in last-in-first-out order" in {
//    val stack = new Stack[Int]
//    stack.push(1)
//    stack.push(2)
//    stack.pop() should be (2)
//    stack.pop() should be (1)
//  }
//
//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }

  "A linkedlist build from array" should "have the same length as the array" in {
    val input = Array(1,2,3)
    val head = Node.buildFromArray(input)

    var i = 0
    var n: Node = head.get
    while(Option(n).isDefined) {
      i += 1
      n = n.next
    }
    i shouldBe input.length
  }

  "A linkedlist build from array" should "contain same elements in same order" in {
    val input = Array(1,2,3)
    val head = Node.buildFromArray(input)

    var i = 0
    var n: Node = head.get
    while(Option(n).isDefined) {
      n.data shouldBe input(i)
      i += 1
      n = n.next
    }
  }

  "A linkedlist build from empty array" should "be None" in {
    val input = Array[Int]()
    val head = Node.buildFromArray(input)
    head shouldBe None
  }

  "A linkedlist build from null array" should "be None" in {
    val input: Array[Int] = null
    val head = Node.buildFromArray(input)
    head shouldBe None
  }

  "Comparing null and null" should "be true" in {
    Node.compareLists(null, null) shouldBe true
  }

  "Comparing null and not null" should "be false" in {
    Node.compareLists(null, new Node(1, null)) shouldBe false
  }

  "Comparing non null and null" should "be false" in {
    Node.compareLists(new Node(1, null), null) shouldBe false
  }

  "Comparing lists of 1 node with same values" should "be true" in {
    Node.compareLists(new Node(1, null), new Node(1, null)) shouldBe true
  }

  "Comparing lists of 1 node with different values" should "be false" in {
    Node.compareLists(new Node(1, null), new Node(2, null)) shouldBe false
  }

  "Comparing lists of more than 1 node with same values" should "be true" in {
    val head = Node.buildFromArray(Array(1,2,3)).get
    Node.compareLists(head, head) shouldBe true
  }

  "Comparing lists of more than 1 node with different last values" should "be false" in {
    val a = Node.buildFromArray(Array(1,2,3)).get
    val b = Node.buildFromArray(Array(1,2,4)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of more than 1 node with different last values 2" should "be false" in {
    val a = Node.buildFromArray(Array(1,2,4)).get
    val b = Node.buildFromArray(Array(1,2,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of more than 1 node with different middle values" should "be false" in {
    val a = Node.buildFromArray(Array(1,2,3)).get
    val b = Node.buildFromArray(Array(1,4,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of more than 1 node with different middle values 2" should "be false" in {
    val a = Node.buildFromArray(Array(1,4,3)).get
    val b = Node.buildFromArray(Array(1,2,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of more than 1 node with different first values" should "be false" in {
    val a = Node.buildFromArray(Array(4,2,3)).get
    val b = Node.buildFromArray(Array(1,2,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of more than 1 node with different first values 2" should "be false" in {
    val a = Node.buildFromArray(Array(1,2,3)).get
    val b = Node.buildFromArray(Array(4,2,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of different length" should "be false" in {
    val a = Node.buildFromArray(Array(1,2,3)).get
    val b = Node.buildFromArray(Array(1,2)).get
    Node.compareLists(a, b) shouldBe false
  }

  "Comparing lists of different length 2" should "be false" in {
    val a = Node.buildFromArray(Array(1,2)).get
    val b = Node.buildFromArray(Array(1,2,3)).get
    Node.compareLists(a, b) shouldBe false
  }

  "The lenght of a null list" should "be 0" in {
    Node.length(null) shouldBe 0
  }

  "The lenght of a 1 element list" should "be 1" in {
    Node.length(new Node(1, null)) shouldBe 1
  }

  "The lenght of a 3 elements list" should "be 3" in {
    Node.length(Node.buildFromArray(Array(1,2,3)).get) shouldBe 3
  }

}
