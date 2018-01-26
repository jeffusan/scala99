package primes

object Primes {

  def isPrime(n: Long): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else
      !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n % _ == 0))
  }

  //euclidien algorithm
  // r_{k-2}=q_{k}r_{k-1}+r_{k}

  def gcd(a: Int, b: Int): Int = {
    if(a < b) gcd(b, a)
    else {
      val r = a % b
      if(r == 0) b
      else gcd(b, r)
    }
  }

  // coprimes: gcd == 1
  // no prime divide both a and b
  // there exist x and y such as ax + by = 1 (Bezout identity) (extended euclidien algorithm)
  // The integer b has a multiplicative inverse modulo a, meaning that there exists an integer y such that by ≡ 1 (mod a).

  // a congruent to b modulo n if a - b divisible by n.
  // modular multiplicative inverse of a: integer x such as ax congruent to 1 modulo m
  // => reminder of ax by m = 1

  def extendedGcd(a: Int, b: Int): (Int, Int, Int) = {

    def go(i: Int, j: Int, s: Int, t: Int, sp: Int, tp: Int): (Int, Int, Int) = {
      val q = i / j
      val r = i % j
      val sn = sp - q*s
      val tn = tp - q*t

      if(r == 0) (q, sn, tn)
      else go(j, r, sn, tn, s, t)
    }

    if(a < b) extendedGcd(b, a)
    else go(a, b, 0, 1, 1, 0)
  }

  // Fermat's little theorem states that if p is a prime number, then for any integer a, the number a^(p) − a is an integer multiple of p.
  // In the notation of modular arithmetic, this is expressed as a^p ≡ a (mod p).
  // For example, if a = 2 and p = 7, 2^7 = 128, and 128 − 2 = 7 × 18 is an integer multiple of 7

  /**
    * If a is not divisible by p, Fermat's little theorem is equivalent to the statement that a^(p − 1) − 1 is an integer multiple of p, or in symbols
    *  a^{p-1} ≡ 1 mod(p)
    *  For example, if a = 2 and p = 7 then 2^6 = 64 and 64 − 1 = 63 is thus a multiple of 7.
    */


}
