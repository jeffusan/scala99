package datastructure.advancedList

import datastructure.advancedLists.Binary
import datastructure.advancedLists.Binary.{Leaf, Node, One, Zero}
import org.scalatest.{FlatSpec, Matchers}

class BinarySpec extends FlatSpec with Matchers {

  "adding" should "handle carrying" in {
    val a = List(0,1,1,1,1) // 30
    val b = List(1,1,0,1,0) // 11
    Binary.add(a, b) shouldBe List(1, 0, 0, 1, 0, 1)
  }

  "multiplying" should "be correct" in {
    val a = List(1,0,1) // 5
    val b = List(0,1,1) // 6
    Binary.multiply(a, b) shouldBe List(0, 1, 1, 1, 1) // 30

    val c = List(1,1,1) // 7
    val d = List(1,0,1) // 5
    Binary.multiply(c, d) shouldBe List(1, 1, 0, 0, 0, 1) // 35
  }

  "increment" should "increment" in {
    // LSB at the head
    Binary.increment(List(1, 0, 1)) shouldBe List(0, 1, 1)
    Binary.increment(List(0, 1)) shouldBe List(1, 1)
    Binary.increment(List(0, 0, 1)) shouldBe List(1, 0, 1)
    Binary.increment(List(1, 1, 1)) shouldBe List(0, 0, 0, 1)
  }

  "addition" should "add" in {
    Binary.addition(List(1), List(0)) shouldBe List(1)
    Binary.addition(List(0, 1), List(0, 1)) shouldBe List(0, 0 , 1)
    Binary.addition(List(1, 0, 1), List(0, 1, 1)) shouldBe List(1, 1, 0, 1)
  }

  "lifting from int to trees" should "work" in {
    Binary.lift(List(5, 4, 3, 2, 1)) shouldBe List(One(Leaf(5)), Zero, One(Node(Node(Leaf(4), Leaf(3)), Node(Leaf(2), Leaf(1)))))
  }

  "searching" should "work" in {
    val list = Binary.lift(List(5, 4, 3, 2, 1))
    //List(One(Leaf(5)), Zero, One(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))))

    val tree = Binary.lookup(list, 4)
    val result = Binary.search(tree, 3)
    result shouldBe 1

    val tree2 = Binary.lookup(list, 0)
    val result2 = Binary.search(tree2, 0)
    result2 shouldBe 5
  }

  "decrement" should "work" in {
    val in = List(0, 0, 1) // 4
    Binary.decrement(in) shouldBe List(1, 1) // 3
  }

  "removeTree" should "return a leaf in a 1 element list" in {
    val in = List(One(Leaf(4)))
    Binary.removeTree(in) shouldBe (Leaf(4), Nil)
  }

  "removeTree" should "return a leaf and a zero list in a 3 elements list" in {
    val shared = One(Node(Leaf(2), Leaf(3)))
    val in = List(One(Leaf(1)), shared)
    Binary.removeTree(in) shouldBe (Leaf(1), List(Zero, shared))
  }

  "removeTree" should "return a leaf and a One list in a 2 elements list" in {
    val in = Binary.lift(List(1, 2))
    Binary.removeTree(in) shouldBe (Leaf(1), List(One(Leaf(2))))
  }

  "head" should "work" in {
    val in = Binary.lift(List(1, 2))
    Binary.head(in) shouldBe 1
  }

  "tail" should "work" in {
    val in = Binary.lift(List(1, 2))
    Binary.tail(in) shouldBe List(One(Leaf(2)))
  }

  "update" should "work" in {
    val in = Binary.lift(List(1, 2, 3, 4))
    Binary.setVal(0, 9, in) shouldBe Binary.lift(List(9, 2, 3, 4))
    Binary.setVal(1, 9, in) shouldBe Binary.lift(List(1, 9, 3, 4))
    Binary.setVal(2, 9, in) shouldBe Binary.lift(List(1, 2, 9, 4))
    Binary.setVal(3, 9, in) shouldBe Binary.lift(List(1, 2, 3, 9))

    val jn = Binary.lift(List(1, 2, 3))
    Binary.setVal(0, 9, jn) shouldBe Binary.lift(List(9, 2, 3))
    Binary.setVal(1, 9, jn) shouldBe Binary.lift(List(1, 9, 3))
    Binary.setVal(2, 9, jn) shouldBe Binary.lift(List(1, 2, 9))
  }

}
