package datastructure

object Heap {

  def parent(i: Int): Int = i / 2

  def left(i: Int): Int = 2 * i + 1

  def right(i: Int): Int = 2 * (i + 1)

  /**
    * min-heap property: h(parent(i)) <= h(i)
    * O(log(n))
    */
  def minHeapify(h: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)
    val smallest0 =
      if (l <= h.size && h(l) < h(i)) l
      else i
    val smallest =
      if (r <= h.size && h(r) < h(smallest0)) r
      else smallest0

    if(smallest != i) {
      val u = swap(h, i, smallest)
      minHeapify(u, smallest)
    } else
      h
  }

  /**
    * max-heap property: h(parent(i)) >= h(i)
    * O(log(n))
    */
  def maxHeapify(h: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)
    val max0 = if (l < h.size && h(i) < h(l)) l else i
    val max = if (r < h.size && h(max0) < h(r)) r else max0

    if (max != i) {
      val u = swap(h, i, max)
      maxHeapify(u, max)
    } else {
      h
    }
  }

  /**
    * elements from a.length / 2 to a.length are leaves:
    * i leaf <=> left(i) outside of range
    * <=> left(i) > n - 1
    * <=> 2*i + 1 > n - 1
    * <=> i > n/2 - 1
    *
    * O(n)
    */
  def buildMaxHeap(a: Array[Int]): Heap = {
    val init = Heap(a, a.length)
    ((a.length / 2 - 1) to 0 by -1).foldLeft(init) {
      (h, i) => maxHeapify(h, i)
    }
  }

  def swap(h: Heap, i: Int, j: Int): Heap = {
    val a = h.a
    val tmp = a(i)
    val u0 = a.updated(i, a(j))
    val u = u0.updated(j, tmp)
    Heap(u, h.size)
  }

}

case class Heap(a: Array[Int], size: Int) {
  def apply(x: Int): Int = a(x)
}
