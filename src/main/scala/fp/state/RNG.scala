package fp.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /**
    * class Foo[+A] // A covariant class
    * some class List[+A], making A covariant implies that for two types A and B where A is a subtype of B,
    * then List[A] is a subtype of List[B]
    * http://docs.scala-lang.org/tutorials/tour/variances.html
    */
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (aa, raa) = ra(rng)
      val (bb, rbb) = rb(raa)
      (f(aa, bb), rbb)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((List[A](), rng)) { (e, o) =>
        val (a, r) = e(o._2)
        (a :: o._1, r)
      }
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]())) ((f, acc) => map2(f, acc)(_ :: _))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * Int.Minvalue is 1 smaller than -(Int.MaxValue),
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, r) = rng.nextInt
    if (a == Int.MinValue) nonNegativeInt(r)
    else if (a < 0) (-a, r)
    else (a, r)
  }

  /**
    * double between 0 and 1
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d = (i/Int.MaxValue).toDouble
    (d,r)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => (i/Int.MaxValue).toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d, r2) = double(r)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d,i), r2)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d0, r0) = double(rng)
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    ((d0,d1,d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def go(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (i,r) = rng.nextInt
        val newAcc = i :: acc
        go(count -1, newAcc)(r)
      }
    }
    go(count, List[Int]())(rng)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    val rands = List.fill(count)(int)
    sequence(rands)
  }

}