package arithmetic

class S99Int(val start: Int) {

  def isPrime: Boolean = {
    if (start < 2) false
    else if (start == 2 ) true
    else if (start % 2 == 0) false
    else {
      var result = true

      val sqrt: Double = Math.sqrt(start)

      for (i <- 3 to sqrt.toInt by 2) {
        if (start % i == 0) {
          println("divisible by " + i)
          result = false
        }
      }
      result
    }
  }


  def isCoprimeTo(i: Int): Boolean = S99Int.gcd(start, i) == 1

  def totient: Int = {
    val wrapped = new S99Int(start)
    (1 to start).count(wrapped.isCoprimeTo )
  }

  def primeFactors: List[Int] = {
    (1 to start).filter(start % _ == 0).filter(new S99Int(_).isPrime).toList
  }


}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def main(args: Array[String]) {
//    println(234234239.isPrime)

//    println(35.isCoprimeTo(64))
//    println(10.totient)
    println(315.primeFactors)
  }

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   * Use Euclid's algorithm.
   * The version of the Euclidean algorithm described above (and by Euclid) can take many subtraction steps
   * to find the GCD when one of the given numbers is much bigger than the other.
   * A more efficient version of the algorithm shortcuts these steps, instead replacing the larger
   * of the two numbers by its remainder when divided by the smaller of the two. With this improvement,
   * the algorithm never requires more steps than five times the number of digits (base 10) of the smaller integer.
   * This was proven by Gabriel Lamé in 1844, and marks the beginning of computational complexity theory.
   */
  def gcd(i: Int, j: Int): Int = {

    if (i > j) gcd(j, i)
    else if(i == 0) j
    else {
      val mod = j % i
      gcd(i, mod)
    }



  }
}