package fp.parallelism

import java.util.concurrent.Executors

import org.scalatest.{FlatSpec, Matchers}

class NonBlockingSpec extends FlatSpec with Matchers {

  "choiceN" should "choose first" in {

    val first = NonBlocking.Par.unit(0)

    val a = NonBlocking.Par.unit("a")
    val b = NonBlocking.Par.unit("b")

    val result = NonBlocking.Par.choiceN(first)(List(a, b))

    NonBlocking.Par.run(Executors.newFixedThreadPool(4))(result) shouldBe "a"

  }

}
