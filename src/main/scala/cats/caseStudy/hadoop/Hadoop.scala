package cats.caseStudy.hadoop

import cats.Monoid
import cats.syntax.monoid._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Hadoop {

  import scala.languageFeature.higherKinds

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A ⇒ B): B = {
    seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
  }

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {

    val coreNumber = Runtime.getRuntime.availableProcessors()
    val groups = values.grouped(coreNumber)

    //foreach group we start a Future and apply the function of the As inside of the Future.
    val futures: Iterator[Future[B]] = groups.map { group ⇒
      Future(foldMap(group)(f))
    }

    //We reduce the Futures of Bs to 1 Future, then reduce the Bs through the monoid for Bs.
    Future.sequence(futures).map {
      iterable ⇒ iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }


  def main(args: Array[String]): Unit = {

    import cats.instances.string._
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))


    (1 to 10).toList.grouped(3).toList

    println(Runtime.getRuntime.availableProcessors)
  }

}
