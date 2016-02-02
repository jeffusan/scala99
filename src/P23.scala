object P23 {

  import P20.removeAtInt
  import P20.removeAt

  def main(args: Array[String]) {
    println(randomSelect2(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  }

  def randomSelect2(x:Int, list: List[Symbol]): List[Symbol] = {
    val array = new Array[Symbol](x)
    val r = scala.util.Random
    for (i <-0 until x ){
      array(i) = list(r.nextInt(list.length))
    }
    array.toList
  }

  def randomSelect(x:Int, list: List[Symbol]): List[Symbol] = {
    if(x <= 0 ) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(list.length), list)
      e :: randomSelect(x - 1, rest)
    }

  }

  def randomSelectInt(x:Int, list: List[Int]): List[Int] = {
    if (x <= 0) Nil
    else {
      val (rest, e) = removeAtInt((new util.Random).nextInt(list.length), list)
      e :: randomSelectInt(x - 1, rest)
    }
  }


  }
