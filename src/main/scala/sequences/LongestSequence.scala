package sequences

import scala.annotation.tailrec
import scala.io.StdIn

object LongestSequence {

  def main(args: Array[String]): Unit = {
    val in = read
    println(longestSequence(in).size)
  }

  def read: Stream[Int]= {

    @tailrec
    def reread(xs: Stream[Int]): Stream[Int] = {
      val s = Option(StdIn.readLine())
      println(s)
      if (s.isEmpty) xs else reread(s.get.toInt +: xs)
    }

    reread(Stream[Int]())
  }

  def longestSequence(seq: Stream[Int]): Seq[Int] = {

    def next(m: Array[Int], start: Int, v: Int): Int = {
      var i = start
      while(i > 0 && seq(m(i)) > v) {
        i = i - 1
      }
      i
    }

    val analysis = seq.zipWithIndex.foldLeft((Array[Int](-1), Array.fill(seq.size + 1)(0), 1)) {
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
          val newP = if (n > 0) p :+ m(n) else p :+ -1
          (newP, m, newMax)
        }
    }

    val last = analysis._2(analysis._3)
    var r = Seq(seq(last))
    var p = analysis._1(last)
    while(p != -1) {
      r = r :+ seq(p)
      p = analysis._1(p)
    }
    r.reverse
  }

}
