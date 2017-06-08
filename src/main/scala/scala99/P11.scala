package scala99

object P11 {
  import P09.pack
  def main(args: Array[String]) {
    println(concatDup(List('a','b','c','a','b','c','a','d')))
  }

  def concatDup(list: List[Char]): List[Any] = pack(list) map {
    e => {
      if(e.length > 1)
        (e.length, e.head)
      else
        e.head
}
}

  def concatDupTypesafe(list: List[Char]): List[Either[Char,(Int, Char)]] = pack(list) map {
    e => {
      if(e.length > 1)
        Right((e.length, e.head))
      else
        Left(e.head)
    }
  }

}
