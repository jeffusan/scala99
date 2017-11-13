package datastructure

import org.scalatest.{FlatSpec, Matchers}

class HeapSpec extends FlatSpec with Matchers {

  "heapifying" should "conserve heap property" in {
    val in = Array(1,2,3,4,5,6,7,8)
    val result = Heap.buildMaxHeap(in)
    (0 until in.length / 2).foreach { i =>
      assert(result.a(i) > result.a(Heap.left(i)))
      val rightI = Heap.right(i)
      if(rightI <= result.a.length - 1)
        assert(result.a(i) > result.a(rightI))
    }
  }

  "heapifying" should "conserve heap property 2" in {
    val in = Array(1,2,3,4,5,6,7,8).reverse
    val result = Heap.buildMaxHeap(in)
    (0 until in.length / 2).foreach { i =>
      assert(result.a(i) > result.a(Heap.left(i)))
      val rightI = Heap.right(i)
      if(rightI <= result.a.length - 1)
        assert(result.a(i) > result.a(rightI))
    }
  }

}
