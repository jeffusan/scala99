
object P27 {

  import P26.combinations


  //  a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
//    Example:
//
//    scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//  res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

//  b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
//
//  Example:
//
//    scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//  res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
//  Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
//
//  You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

  def main(args: Array[String]) {
//    println(group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
    println(groupn(List(4, 3, 2), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
  }

  def group3(ls: List[String]): List[List[List[String]]] = {
    if (ls.length != 9) throw new Exception

    // get combinations of 4: foreach ls4, rest1 = ls.sublist(ls4)
    // foreach rest1, get combinations of 3: foreach ls3, rest2 = rest1.sublist(ls3)
    // foreach rest2, get combinations of 2
    // aggregate results

    val result = combinations(4, ls).flatMap(
      comb4 => combinations(3, ls.filter(e => !comb4.contains(e)))
        .flatMap(
          comb3 => combinations(2, ls.filter(e => !comb4.contains(e) && !comb3.contains(e)))
            .map(
              comb2 => List(comb4, comb3, comb2)
            )
        )
    )

    result

  }

  def groupn(nLs: List[Int], ls: List[String]): List[List[List[String]]] = {

    if(nLs.length == 0) List(Nil)
    else {
      combinations(nLs(0), ls).flatMap(
       comb => groupn(nLs.tail, ls.filter(e => !comb.contains(e))).map (comb :: _)
      )

    }
  }

//  def group3_solution[A](ls: List[A]): List[List[List[A]]] =
//    for {
//      a <- combinations(2, ls)
//      noA = ls -- a
//      b <- combinations(3, noA)
//    } yield List(a, b, noA -- b)

}
