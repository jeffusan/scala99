package cats

import cats.data.Writer

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MonadTransformer {

  import scala.concurrent.Future
  import cats.data.{EitherT, OptionT}

  type Error = String
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  type Logged[A] = Writer[List[String], A]
  // Example method that returns nested monads:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }


  // Example combining multiple calls to parseNumber:
  def addNumbers(
                  a: String,
                  b: String,
                  c: String
                ): Logged[Option[Int]] = {
    import cats.data.OptionT
    import cats.instances.all._


    // Transform the incoming stacks to work on them:
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    // Return the untransformed monad stack:
    result.value
  }

  type Response[A] = Future[Either[String, A]]

  type Response2[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response2[Int] = {
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global

    powerLevels.get(autobot) match {
      case Some(l) ⇒ EitherT.right(Future(l))
      case None ⇒ EitherT.left(Future(s"no level for $autobot"))
    }
  }

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10,
    "aa"   -> 1
  )

  def canSpecialMove(
    ally1: String,
    ally2: String
  ): Response2[Boolean] = {
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global

    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield a + b > 15
  }

//  def tacticalReport(
//                      ally1: String,
//                      ally2: String
//                    ): String = {
//    Await.result(canSpecialMove(ally1, ally2).value, Duration.Inf) match {
////      case   ...
//    }
//  }




  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    import cats.syntax.applicative._

    val answer: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

//    println(answer.value)

    val result1 = addNumbers("1", "2", "3")
//    println(result1)
    // result1: Logged[Option[Int]] = WriterT((List(Read 1, Read 2, Read 3),Some(6)))
    val result2 = addNumbers("1", "a", "3")
//    println(result2)

    println(getPowerLevel("aa"))



  }

}
