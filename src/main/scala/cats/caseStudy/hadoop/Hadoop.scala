package cats.caseStudy.hadoop

import cats.Monoid
import cats.syntax.monoid._

object Hadoop {

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A ⇒ B): B = {
    seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
  }


  def main(args: Array[String]): Unit = {


    import cats.instances.string._
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  }

}
