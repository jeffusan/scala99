package sort

object HeapSort {

  def sort(a: Array[Int]): Array[Int] = {
    val h = buildMaxHeap(a)
    val o = (a.length - 1 to 1 by -1).foldLeft(h) {
      (r,i) =>
        val h0 = r.swap(i, 0)
        val h1 = new Heap(h0.a, h0.heapSize - 1)
        maxHeapify(h1, 0)
    }
    o.a
  }

  /**
    * O(log(n))
    */
  def maxHeapify(h: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)
    val largest0 =
      if(l <= h.heapSize && h(l) > h(i)) l
      else i
    val largest =
      if(r <= h.heapSize && h(r) > h(largest0)) r
      else largest0
    if(i != largest) {
      val u = h.swap(i, largest)
      maxHeapify(u, largest)
    } else
      h
  }

  /**
    * O(log(n))
    */
  def minHeapify(h: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)
    val smallest0 =
      if(l <= h.heapSize && h(l) < h(i)) l
      else i
    val smallest =
      if(r <= h.heapSize && h(r) < h(smallest0)) r
      else smallest0
    if(i != smallest) {
      val u = h.swap(i, smallest)
      minHeapify(u, smallest)
    } else
      h
  }

  class Heap(val a: Array[Int], val heapSize: Int) {

    def apply(i: Int): Int = a(i)
    def swap(i: Int, j: Int): Heap = {
      val  t = a(i)
      a.update(i, a(j))
      a.update(j, t)
      new Heap(a, heapSize)
    }
  }

  /**
    * O(n)
    */
  def buildMaxHeap(a: Array[Int]): Heap = {
    //because all elements from size / 2 are leaves (left = i * 2)
    ((a.length / 2) to 0 by -1).foldLeft(new Heap(a, a.length -1)) {
      (h, i) => maxHeapify(h, i)
    }
  }

  def parent(i: Int): Int = (i - 1) / 2
  def left(i: Int): Int = 2 * i + 1
  def right(i: Int): Int = 2 * i + 2

}
