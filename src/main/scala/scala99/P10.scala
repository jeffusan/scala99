package scala99

object P10 {
  import P09.pack

  def main(args: Array[String]) {
    println(encode(List('a','b','c','a','b','c','a')))
    println(pack(List('a','b','c','a','b','c','a')))
    println(encodee(List('a','b','c','a','b','c','a')))

  }

  def encodee[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head) }


  def encode(list: List[Char]) : List[(Int, Char)] = {

    def encodeTail(list: List[Char], res:List[(Int, Char)]): List[(Int, Char)] = {
      if(list == Nil) List()
      else{
        val count: Int = list.foldLeft(0) {(counter, element) => {
          if (list.head == element )  counter +1
          else counter
        }}

        val (packed, next) = list.partition (_ == list.head)
//        if(next == Nil) (count, list.head) :: res
        if(next == Nil)  res :+ (count, list.head)
        else {
//          val tmp = (count, list.head) :: res
          val tmp =  res :+ (count, list.head)
          encodeTail(next, tmp)
        }

      }
    }

    encodeTail(list, List())

  }
}
