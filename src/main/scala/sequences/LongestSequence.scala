package sequences

object LongestSequence {

  private val Continue = 1
  private val Stop = -1

  def longestSequence(seq: Seq[Int]): Seq[Int] = {


    val analysis = seq.zipWithIndex.foldLeft((Seq[Int](-1, -1), Seq[Int](-1, 0))) {
      (a, e) =>
        val p = a._1
        val m = a._2

        val i = e._1
        val v = e._2

        val mReversed = m.reverse
        val head = seq(mReversed.head)
        if (head.compareTo(v) <= 0) {
          val newP = p.+:(mReversed.head)
          val newM = m.+:(i)
          (newP, newM)
        } else {
          val x = mReversed.indexWhere(seq(_) <= v)
          val inM = reverseIndex(m.length, x)
          val newM = m.updated(inM + 1, i)
          val newP = p.+:(inM)
          (newP,newM)
        }
    }

    val last = analysis._2.last
    var r = Seq(seq(last))
    var p = analysis._1(last)
    while(p != -1) {
      r.+:(seq(p))
      p = analysis._1(p)
    }

    r.reverse

  }

  private def reverseIndex(l: Int, i: Int): Int = {
    if(l % 2 == 0) l - i - 1
    else l - i
  }

}
