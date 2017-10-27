package cats.caseStudy.hadoop

import cats.Monoid

object Hadoop {

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A ⇒ B): B = ???

}
