package datastructure

object Heap {

  def parent(i: Int): Int = i / 2

  def left(i: Int): Int = 2 * i + 1

  def right(i: Int): Int = 2 * (i + 1)

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
