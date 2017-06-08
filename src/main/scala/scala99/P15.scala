package scala99

object P15 {

  def main(args: Array[String]) {

    println(duplicate(3, List('a','b','c')))
    println(duplicate2(3, List('a','b','c')))
  }

  def duplicate(x: Int, list: List[Char]): List[Char] = {
    list flatMap {
      c => {
        var res:List[Char] = List()
        for(i <- 0 until x ){
          res = c :: res
        }
        res
      }
    }
  }

  def duplicate2(x: Int, list: List[Char]): List[Char] = {
    list flatMap {
          //fill: A sequence of length x where each element is computed by the expression.
      c => List.fill(x) {c}
    }
  }

}
