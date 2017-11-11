package fp.testing

import fp.state.RNG.SimpleRNG
import fp.state.{RNG, State}
import fp.testing.Prop._

trait Prop { self =>
  def &&(p: Prop): Prop = new Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  }
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}


case class Gen[A](sample: State[RNG,A])

object Gen {

  implicit def ops[A](a: Gen[A]): GenOps[A] = new GenOps(a)

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A,B) => C): Gen[C] = {
    val sc = for {
      sa <- a.sample
      sb <- b.sample
    } yield f(sa, sb)
    Gen(sc)
  }

  def map[A, B](a: Gen[A])(f: A => B): Gen[B] = {
    Gen(a.sample.map(f(_)))
  }

  def flatMap[A, B](a: Gen[A])(f: A => Gen[B]): Gen[B] = {
    Gen(a.sample.flatMap(a => f(a).sample))
  }

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val a = choose(start, stopExclusive)
    val b = choose(start, stopExclusive)
    map2(a, b)((_,_))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => if(n % 2 == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val rng = SimpleRNG(1324)

    val (endRng, result) = (0 until n).foldLeft[(RNG, List[A])]((rng, List[A]())){
      (rngAndList, _) =>
        val r = rngAndList._1
        val list = rngAndList._2
        val (a, newRng) = g.sample.run(r)
        (newRng, a :: list )
    }
    Gen(State.unit(result))
  }

  def toOption[A](a: Gen[A]): Gen[Option[A]] = {
    Gen(a.sample.map(Option(_)))
  }

  def string: String = {
    val genChar: Gen[Char] = choose('a'.toInt, 'z'.toInt).map(_.toChar)

    val stringGen = choose(1, 100).flatMap(i => listOfN(i, genChar).map(_.mkString))

    val (s, r) = stringGen.sample.run(SimpleRNG(1324))
    s
  }
}

class GenOps[A](a: Gen[A]) {
  def map[B](f: A => B): Gen[B] = Gen.map(a)(f)
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(a)(f)
}


