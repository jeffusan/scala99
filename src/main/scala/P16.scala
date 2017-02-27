object P16 {

  def main(args: Array[String]) {
    println(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
    println(drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    println(dropFunctional(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

  def drop(x:Int, list:List[Char]): List[Char] ={

    val result  = list.foldRight(new ListAndCounter(List(), 0)) { (elt: Char, tmp:ListAndCounter) => {

      var count = tmp.counter +1;

      var listTmp = tmp.list
      if(!(count % x == 0 )){

        listTmp = elt :: listTmp
      }
      new ListAndCounter(listTmp, count)

      //      val res: List[Char] = tmp.list
      //      res
      //      tmp

    }

    }
    result.list

  }

  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }


}

case class ListAndCounter(list: List[Char], counter: Int) {

}
