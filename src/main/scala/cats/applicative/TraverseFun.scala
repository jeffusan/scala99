package cats.applicative

import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object TraverseFun {

  import cats.Applicative
  import cats.instances.future._
  import cats.syntax.applicative._

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)


  private val accumulator = List.empty[Int].pure[Future]

  def oldCombine(
                  accum : Future[List[Int]],
                  host  : String
                ): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum  <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }

  import cats.syntax.cartesian._

  def newCombine(
                  accum: Future[List[Int]],
                  host: String
                ): Future[List[Int]] =
    (accum |@| getUptime(host)).map(_ :+ _)

  import scala.language.higherKinds
  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum |@| func(item)).map(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  Await.result(
    listTraverse(hostnames)(getUptime),
    1.second
  )

  import cats.instances.option._
  def process(inputs: List[Int]) =
    listTraverse[Option, Int, Int](inputs)(n => if(n % 2 == 0) Some(n) else None)

  import cats.data.Validated
  import cats.instances.list._ // Applicative[ErrorsOr] needs a Monoid[List]
  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }


  def main(args: Array[String]): Unit = {
    import cats.instances.vector._

    /**
      * The argument is of type List[Vector[Int]],
      * so we’re using the Applicative for Vector and the return type is going to be Vector[List[Int]].
      * Vector is a monad, so its cartesian combine func on is based on flatMap.
      * We’ll end up with a Vector of Lists of all the possible combinations of List(1, 2) and List(3, 4)
      */
    val sequence: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4))) //Vector(List(1, 2, 3, 4))
    println(sequence)

    val seq2 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
    println(seq2)

    println(process(List(2,4,6)))
    println(process(List(1,2,3)))

    println(processValidated(List(2,4,6)))
    println(processValidated(List(1,2,3)))

  }


}
