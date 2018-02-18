package datastructure.graph

import datastructure.graph.Graph.Cycle
import org.scalatest.{FlatSpec, Matchers}

class GraphSpec extends FlatSpec with Matchers {

  "succSet" should "collect successors of a given node" in {
    val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
                    ("n", "q"), ("o", "r"), ("p", "q"),
                    ("q", "r"), ("q", "s"))

    Graph.succSet("m", graph) shouldBe List("n", "o", "p")

    graph.filter(_._1 == "m").map(_._2)  shouldBe List("n", "o", "p")
  }

  "depthfirst" should "traverse to leaves first" in {
    val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
      ("n", "q"), ("o", "r"), ("p", "q"),
      ("q", "r"), ("q", "s"))

    Graph.depthFirst(graph) shouldBe List("m", "n", "q", "r", "s", "o", "p")
  }

  "depthfirst1" should "traverse to leaves first without appending lists" in {
    val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
      ("n", "q"), ("o", "r"), ("p", "q"),
      ("q", "r"), ("q", "s"))

    Graph.depthFirst1(graph) shouldBe List("m", "n", "q", "r", "s", "o", "p")
  }

  "topsort" should "traverse to leaves first without appending lists" in {
    val graph = List(("m", "n"),
      ("n", "o"),
      ("o", "p"),
        ("p", "q"),
          ("q", "v"),
      ("o", "w"),
        ("w", "z"),
          ("z", "v"))

    Graph.topsort(graph) shouldBe List("m", "n", "o", "w", "z", "p", "q", "v")
  }
  
  "topsort" should "sort" in {
    val grwork = List(("getup","shower"),
        ("shower", "breakfast"),
        ("breakfast","dress"),
          ("dress","office"),
            ("office", "dinner"),
        ("breakfast","leisurely_lunch"),
          ("leisurely_lunch", "movie"),
            ("movie", "dinner"))

    val result = Graph.topsort(grwork)
    val expected = List("getup", "shower", "breakfast", "leisurely_lunch", "movie", "dress", "office", "dinner")

    result shouldBe expected
  }

  "topsort" should "sort when bad ordering" in {
    val grwork = List(
      ("shower", "breakfast"),
      ("getup","shower"),
      ("breakfast","dress"),
      ("dress","office"),
      ("office", "dinner"),
      ("breakfast","leisurely_lunch"),
      ("leisurely_lunch", "movie"),
      ("movie", "dinner"))

    val result = Graph.topsort(grwork)
    val expected = List("getup", "shower", "breakfast", "leisurely_lunch", "movie", "dress", "office", "dinner")

    result shouldBe expected
  }

  "topsortWithCycle" should "sort and detect cycle" in {
    val grwork = List(("getup","shower"),
      ("shower", "breakfast"),
      ("breakfast","dress"),
      ("dress","office"),
      ("office", "dinner"),
      ("breakfast","leisurely_lunch"),
      ("leisurely_lunch", "movie"),
      ("movie", "dinner"),
      ("dinner", "movie"))

    val result = Graph.topsortOrCycle(grwork)
    val expected = Left(Cycle("dinner"))

    result shouldBe expected
  }

}
