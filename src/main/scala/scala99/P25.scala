package scala99

object P25 {

  import P23.randomSelect

  def main(args: Array[String]) {
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  }

  def randomPermute2(ls: List[Symbol]): List[Symbol] = randomSelect(ls.length, ls)

  def randomPermute(ls: List[Symbol]): List[Symbol] = {
    val rand = new util.Random
    val a = ls.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }


  }
