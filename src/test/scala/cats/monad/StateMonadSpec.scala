package cats.monad

import org.scalatest.{FlatSpec, Matchers}

class StateMonadSpec extends FlatSpec with Matchers {

  it should "eval" in {
    StateMonad.evalOne("4").run(Nil).value._2 shouldBe 4
  }

  it should "add" in {
    val program = for {
      _   <- StateMonad.evalOne("1")
      _   <- StateMonad.evalOne("2")
      ans <- StateMonad.evalOne("+")
    } yield ans

    program.run(Nil).value._2 shouldBe 3
  }

  it should "eval all" in {
    StateMonad.evalAll(List("2", "4", "-")).run(Nil).value._2 shouldBe 2
  }

}
