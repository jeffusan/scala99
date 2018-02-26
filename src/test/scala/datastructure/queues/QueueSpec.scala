package datastructure.queues

import datastructure.queues.PriorityQueue.{Leaf, Node}
import org.scalatest.{FlatSpec, Matchers}

class QueueSpec extends FlatSpec with Matchers {

  "lifting" should "work" in {
    PriorityQueue.lift(List(1, 2, 3)) shouldBe Node(2, 1, Node(1, 2, Leaf, Leaf), Node(1, 3, Leaf, Leaf))

  }

  "merging" should "merge" in {

    val a = PriorityQueue.lift(List(1, 2, 3))
    val b = PriorityQueue.lift(List(4, 5, 6))

    val r = PriorityQueue.merge(a, b)

    r shouldBe Node(4, 1, Node(3, 3, Node(2, 4, Node(1, 5, Leaf, Leaf), Node(1, 6, Leaf, Leaf)), Leaf), Node(1, 2, Leaf, Leaf))

    val x = PriorityQueue.merge(b, Node(1, 7, Leaf, Leaf))
    x shouldBe Node(3, 4, Node(2, 6, Node(1, 7, Leaf, Leaf), Leaf), Node(1, 5, Leaf, Leaf))

    val y = PriorityQueue.merge(b, Node(1, 2, Leaf, Leaf))
    y shouldBe Node(3, 2, Node(2, 4, Node(1, 5, Leaf, Leaf), Node(1, 6, Leaf, Leaf)), Leaf)

  }

}
