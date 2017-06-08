package scala99

object P08 {

  def main(args: Array[String]) {
    println(removeConseq(List('a','a','b','c','e','e','z','z','a')))
    println(compress(List('a','a','b','c','e','e','z','z','a')))
  }


  def removeConseq(list: List[Char]): List[Char] = {
    var res : List[Char] = List()
    for( (x,i) <- list.view.zipWithIndex ) {
//      println(list(i))
      if( i < list.length -2) {
        if (x != list(i + 1)){
//          println(x)
          res =  res ::: List(x)
//          println(res)
        }
      }else if (i == list.length -1){
        res =  res ::: List(x)
      }
    }
    res
  }

  def compress(list: List[Char]): List[Char] = {
    list.foldRight(List[Char]()) { (element, tmpList) =>
      if(tmpList.isEmpty || tmpList.head != element) element :: tmpList
      else tmpList
    }
  }

}
