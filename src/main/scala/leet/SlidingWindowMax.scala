package leet

import datastructure.Heap


/**
  * Given an array nums, there is a sliding window of size k
  * which is moving from the very left of the array to the very right.
  * You can only see the k numbers in the window.
  * Each time the sliding window moves right by one position.
  * For example
  * Given nums = [1,3,-1,-3,5,3,6,7], and k = 3.
  * Window position                Max
  * ---------------               -----
  *[1  3  -1] -3  5  3  6  7       3
  * 1 [3  -1  -3] 5  3  6  7       3
  * 1  3 [-1  -3  5] 3  6  7       5
  * 1  3  -1 [-3  5  3] 6  7       5
  * 1  3  -1  -3 [5  3  6] 7       6
  * 1  3  -1  -3  5 [3  6  7]      7
  *
  * Therefore, return the max sliding window as [3,3,5,5,6,7].
  */
object SlidingWindowMax {

  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {

    val q0 = Vector.fill(k)(0)
    val w = (0 until k).foldLeft(q0) {(qi, i) ⇒ qi.updated(i, nums(i))}
    val queue0 = new Queue(w, k, 0, 0)

    val (maxs, _) = (0 to nums.length - k).foldLeft((Array.fill(nums.length - k  + 1)(0), queue0)) {
      case ((a, queue), i) ⇒
        val max = queue.elements.max
        val au = a.updated(i, max)
        val (_, qu) = queue.dequeue()
        val qu1 = if (i >= nums.length - k) qu else qu.enqueue(nums(i + k))
        (au, qu1)
    }

    maxs
  }

  class Queue[A](val elements: Vector[A], n: Int, head: Int, tail: Int) {
    def enqueue(a: A): Queue[A] = {
      val u = elements.updated(tail, a)
      val nextTail = if(tail == n - 1) 0 else tail + 1
      new Queue(u, n, head, nextTail)
    }

    def dequeue(): (A, Queue[A]) = {
      val a = elements(head)
      val nextHead = if(head == n - 1) 0 else head + 1
      (a, new Queue(elements, n, nextHead, tail))
    }
  }

  class MaxHeap[A](val heap: Heap) {
    
  }

}


