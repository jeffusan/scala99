package disjointset

import scala.annotation.tailrec
import scala.io.StdIn

object ComponentSizes {


  def build(n: Int, in: List[(Int, Int)]): (Int, Int) = {

    val vector = Vector.fill(2 * n + 1)(0)

    val components = in.foldLeft(vector) {
      (v, e) => {
        val l = v(e._1)
        val r = v(e._2)

        (l,r) match {
          case (0,0) => v.updated(e._1, e._1).updated(e._2, e._1)
          case (i, 0) => v.updated(e._2, i)
          case (0, i) => v.updated(e._1, i)
          case (a, b) => v.zipWithIndex.foldLeft(v) {
            (vec, z) =>
              val i = z._2
              val e = z._1
              if(e == b) vec.updated(i, a) else vec
          }
        }
      }
    }

    val counts = components.foldLeft(Map[Int, Int]()) {
      (m, e) =>
        if(e == 0) m
        else {
          m.updated(e, m.getOrElse(e, 0) + 1)
        }
    }
    (counts.values.min,counts.values.max)
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
