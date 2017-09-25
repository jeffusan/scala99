package cats.monad

import cats.data.Writer
import cats.instances.map

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object WriterMonad {
  type Logged[A] = Writer[Vector[String], A]
  //  123.pure[Logged]

  def main(args: Array[String]): Unit = {
    val Vector((logA, ansA), (logB, ansB)) = Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(3).run)
    )), 5.seconds)

    logA.foreach(println)
    logB.foreach(println)


//    println(writer1)
//    println(w2)
  }


  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  val w2 = 10.pure[Logged].flatMap(a ⇒ Vector("a", "b", "c").tell.flatMap(_ ⇒ 32.writer(Vector("x", "y", "z")).map(b ⇒ a + b)))

  Vector("msg1", "msg2", "msg3").tell
  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  a.value
  a.written
  val (log, result) = b.run

  val writer3 = writer1.bimap(
    log    => log.map(_.toUpperCase),
    result => result * 100
  )

  val writer4 = writer1.mapBoth { (log, result) =>
    val log2    = log.map(_ + "!")
    val result2 = result * 1000
    (log2, result2)
  }

  val writer5 = writer1.reset

  val writer6 = writer1.swap

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial0(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial0(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged]
           else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
      } yield ans
  }

}
