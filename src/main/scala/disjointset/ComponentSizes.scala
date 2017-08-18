package disjointset

import scala.annotation.tailrec
import scala.io.StdIn

object ComponentSizes {


  def build(n: Int, in: List[(Int, Int)]): (Int, Int) = {

    val v = Vector.fill(2 * n + 1)(0)
    val (parent, _) = v.foldLeft((v, 0)) {(vi, e) => (vi._1.updated(vi._2,vi._2), vi._2 + 1)}

    val rank = Vector.fill(2 * n + 1)(0)


    /**
      * with heuristic: path compression
      */
    def find(i: Int, p: Vector[Int]): (Int, Vector[Int]) = {
      val u = if(p(i) != i) {
        val (v, pu) = find(p(i), p)
        pu.updated(i, v)
      }
      else p
      (u(i), u)
    }

    val (p, r) = in.foldLeft(parent, rank) {
      (pr, e) => {
        val p = pr._1
        val r = pr._2

        val (px, p1) = find(e._1, p)
        val (py, p2) = find(e._2, p1)

        val p3 = if(r(px) > r(py)) p2.updated(py, px) else p2.updated(px, py)
        /**
          * heuristic “union by rank” to make the set with less elements to point on the one with more elements,
          * so that there is less elements to "update".
          */
        val r1 = if(r(px) == r(py)) r.updated(px, r(px) +1) else r
        (p3, r1)
      }
    }

    val (_, map) = (0 to 2 * n).foldLeft(p, Map[Int, Int]()) {
      (p0map, e) =>
        val p0 = p0map._1
        val map = p0map._2
        val (i, p1) = find(e, p0)
        val m1 = map.updated(i, map.getOrElse(i, 0) + 1)
        (p1, m1)
    }

    val values = map.values.toSet.filter(_ != 1) //remove single elements
    (values.min, values.max)
  }


  def main(args: Array[String]): Unit = {
    val in = read
    val r = build(in._1, in._2)
    println(r._1 + " " + r._2)
  }

  def read: (Int, List[(Int, Int)]) = {

    val size = StdIn.readLine().toInt

    @tailrec
    def reread(i: Int, list: List[(Int, Int)]): List[(Int, Int)] = {
      val s = Option(StdIn.readLine())
      if (s.isEmpty) list else {
        val intsStr = s.get.split(' ')
        val ints = intsStr.map(_.toInt)
        reread(i + 1, list :+ (ints.head, ints(1)))
      }
    }

    val v = reread(0, List())
    (size, v)
  }

}
