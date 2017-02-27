object P24 {

  import P23.randomSelectInt

  def main(args: Array[String]) {
    println(lotto(6, 49))

  }

  def lotto(n: Int, m: Int): List[Int] = randomSelectInt(n, (1 to m).toList)

}
