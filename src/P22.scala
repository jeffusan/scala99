object P22 {

  def main(args: Array[String]) {
    println(range(4, 9))
  }

  def range2(x: Int, y:Int): List[Int] = {
    val r = x until y + 1
    r.map(i => i).toList
  }

  def range(x: Int, y:Int): List[Int] = List.range(x,y + 1)

}
