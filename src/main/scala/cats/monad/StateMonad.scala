package cats.monad

import cats.data.State
import cats.data.State._

object StateMonad {

  val getDemo = State.get[String]

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {

    def operand(n: Int): CalcState[Int] = State[List[Int], Int] {
      stack ⇒ (n :: stack, n)
    }

    def operator(f: (Int, Int) ⇒ Int): CalcState[Int] = State[List[Int], Int] {
      case a :: b :: tail ⇒
        val r = f(a, b)
        (r :: tail, r)
    }

    sym match {
      case "+" ⇒ operator(_ + _)
      case "-" ⇒ operator(_ - _)
      case "*" ⇒ operator(_ * _)
      case "/" ⇒ operator(_ / _)
      case n ⇒ operand(n.toInt)
    }

  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(pure[List[Int], Int](0)) {
      (s, e) ⇒ for {
        _ <- s
        a <- evalOne(e)
      } yield a
    }
  }

  def main(args: Array[String]): Unit = {
    println(getDemo)
    println(getDemo.run("lol").value)

    val setDemo = State.set[Int](30)
    println(setDemo)

    println(setDemo.run(10).value)

//    • get extracts the state as the result;
//    • set updates the state and returns unit as the result;
//    • pure ignores the state and returns a supplied result;
//    • inspect extracts the state via a transforma on func on;
//    • modify updates the state using an update func on.

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)

    val p = get[Int].flatMap(a ⇒ set[Int](a + 1).flatMap(_ ⇒ get[Int]).flatMap(b ⇒ modify[Int](_ + 1).flatMap(_ ⇒ inspect[Int, Int](_ * 1000).map(c ⇒ (a, b, c)))))

    // program: cats.data.State[Int,(Int, Int, Int)] = cats.data.StateT@4996bc50
    val (state, result) = p.run(1).value
    println(state) // 3
    println(result) // 1,2,3000

  }

}
