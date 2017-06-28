package sequences

import scala.annotation.tailrec
import scala.io.StdIn

object LongestSequence {

  def main(args: Array[String]): Unit = {
    val in = read
    println(longestSequence(in))
  }

  def read: Array[Int]= {

    val size = StdIn.readLine().toInt

    val input = Array.ofDim[Int](size)

    @tailrec
    def reread(i: Int): Array[Int] = {
      val s = Option(StdIn.readLine())
      if (s.isEmpty) input else {
        input(i) = s.get.toInt
        reread(i + 1)
      }
    }

    reread(0)
  }

  def longestSequence(seq: Array[Int]): Array[Int] = {

    if(seq.length == 1) seq else {

      def next(m: Array[Int], start: Int, v: Int): Int = {
        var i = start
        while(i > 0 && seq(m(i)) >= v) {
          i = i - 1
        }
        i
      }

      val analysis = seq.zipWithIndex.foldLeft((Array.fill(seq.length)(-1), Array.fill(seq.length + 1)(0), 1)) {
        (a, e) =>
          val p = a._1
          val m = a._2
          val max = a._3

          val v: Int = e._1
          val i = e._2

          if (i == 0) {
            a
          } else {
            val n = next(m, max, v)
            m(n + 1) = i
            val newMax = if(n >= max) n + 1 else max
            if (n > 0) p(i) = m(n) else p(i) = -1
            (p, m, newMax)
          }
      }

      val last = analysis._2(analysis._3)
          var r = Array(seq(last))
//      var r = 1
      var p = analysis._1(last)
      while(p != -1) {
              r = r :+ seq(p)
//        r = r + 1
        p = analysis._1(p)
      }
//      r
          r.reverse
    }
  }

}
