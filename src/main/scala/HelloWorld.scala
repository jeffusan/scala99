object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")

    println(last(List(1,2,3,5)))
  }

  def last(list: List[Int]): Int = {
    return list.last
  }
}
