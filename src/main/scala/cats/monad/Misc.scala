package cats.monad

import cats.Monad
import cats.data.{Writer, WriterT}

object Misc {

  def numbersBetween(min: Int, max: Int): List[Int] =
    (min to max).toList

  type Id[A] = A

  val idMonad = new MonadMisc[Id] {
    def pure[A](a: A): Id[A] = a

    def flatMap[A, B](value: Id[A])(f: (A) => Id[B]): Id[B] = {
      f(value)
    }

    override def map[A, B](value: Id[A])(f: (A) => B): Id[B] = super.map(value)(f)
  }

  def main(args: Array[String]) {
    val a = for {
      x <- numbersBetween(1, 3)
      y <- numbersBetween(4, 5)
    } yield (x,y)

//    println(a)
//
//
//    println(numbersBetween(1, 3).flatMap(x => numbersBetween(4,5).map(y => (x, y))))


    import cats.instances.option._
    import cats.instances.list._
    import scala.language.higherKinds


    val opt1 = Monad[Option].pure(3)

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    // opt2: Option[Int] = Some(5)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    // opt3: Option[Int] = Some(500)
    val list1 = Monad[List].pure(3)
    // list1: List[Int] = List(3)
    val list2 = Monad[List].flatMap(List(1, 2, 3))(x => List(x, x*10))

    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      for {
        x <- a
        y <- b
      } yield x*x + y*y


    import cats.syntax.either._

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if(num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        } }

//    println(countPositive(List(1, 2, 3)))
    // res7: Either[String,Int] = Right(3)
//    println(countPositive(List(1, -2, 3)))
    // res8: Either[String,Int] = Left(Negative. Stopping!)

    "Error".asLeft[Int].getOrElse(0)
    // res9: Int = 0
    "Error".asLeft[Int].orElse(2.asRight[String])
    // res10: Either[String,Int] = Right(2)

//    println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))
    // res11: Either[String,Int] = Left(Must be non-negative!)

    val d = for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if(b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String
        ]
    } yield c * 100
//    println(d)

    factorial(5)

  }

  import cats.Eval

//  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => Eval[B]): Eval[B] =
//    as match {
//      case head :: tail =>
//        Eval.defer(fn(head, foldRight(tail, acc)(fn)))
//      case Nil =>
//        Eval.now(acc)
//    }


  import cats.syntax.writer._

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  // a: cats.data.WriterT[cats.Id,scala.collection.immutable Vector[String],Int] = WriterT((Vector(msg1, msg2, msg3),123))
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))


  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

}



import cats.Eval
import cats.data.{Writer, WriterT}

import scala.language.higherKinds

trait MonadMisc[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A,B](value: F[A])(f: A => B): F[B] = {
    flatMap(value)(a => pure(f(a)))
  }

}