package scala99

object P19 {

  def main(args: Array[String]) {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k);
    println(list)
    println(rotate(3,list))
  }

  def rotate(x: Int, list: List[Symbol]): List[Symbol] = {
    return list.zipWithIndex.foldRight(new Array[Symbol](list.length)) { ( z, res ) => {
      val c = z._1
      val i = z._2

      val pos = if (i + x < list.length) i + x else (i + x - list.length)
      res(pos) = c
      res
    }
    }.toList
  }

}
