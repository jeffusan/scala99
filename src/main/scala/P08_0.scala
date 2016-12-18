object P08_0 {

  def main(args: Array[String]) {
    println(compress(List('a','a','b','c','c','z','a')))
  }

  def compress(list: List[Char]): List[Char] = {
    val check = new Array[Boolean](26)
    val res = list.filter(c => {

      if (!check(c - 'a')) {
        check(c - 'a') = true
        true
      }
      else{
        false
      }
    })

//    check.foreach(println(_))
   res
  }

}
