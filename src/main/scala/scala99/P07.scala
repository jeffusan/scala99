package scala99

object P07 {

  def main(args: Array[String]) {
//    println(List(List(1,2), List(3,4)).flatten)
    println(flatten(List(List(List(1,2),List(1,2)), List(List(1,2),List(1,2)))))
    println(List(List(List(1,2),List(1,2)), List(List(1,2),List(1,2))).flatten)
  }

//  def flatten(list: List[Any]): List[Any] = list.flatMap {
//    case l: List[Any] => l.flatten
//    case e => List(e)
//
//  }


  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}
