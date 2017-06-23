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

  def _map[S,A,B](t: S => (A,S))(f: A => B): S => (B,S) =
    s => {
      val (a, ss) = t(s)
      (f(a), ss)
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


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a,r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThanViaFlatmap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i => {
        val mod = i % n
        if (i + (n-1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThanViaFlatmap(n)
      }
    }
  }

  def mapViaFlatmap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(raa => flatMap(rb)(rbb => unit(f(raa, rbb))))
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
  }


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

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, ss) = run(s)
      (f(a), ss)
    }
  )

  def map2[B, C](t: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, s1) = run(s)
      val (b, s2) = t.run(s1)
      (f(a, b), s2)
    }
  )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
  })
}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
}

object Machine {

  val Unlocked = false
  val Locked = true

  /**
    * Operate the machine based on the list of inputs.
    * Returns the number of coins and candies left in the machine at the end.
    * For example:
    * if the input Machine has 10 coins and 5 candies,
    * and a total of 4 candies are successfully bought,
    * the output should be (14, 1)
    *
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State( s => {

    inputs.foldLeft(((s.coins,s.candies),s)) {
      (o, i) => {
        val m = o._2
        (m.locked, i) match {
          case (true, Coin) if m.coins > 0 => (o._1, Machine(Unlocked, m.candies, m.coins + 1))
          case (false, Turn) =>
            val newMachine = Machine(Locked, m.candies -1, m.coins)
            ((newMachine.coins, newMachine.candies), newMachine)
          case _ => o
        }
      }
    }
  })



}

