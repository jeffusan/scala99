package logic

import language.implicitConversions

class S99Logic(a: Boolean) {

  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }

  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _         => false
  }

}

object S99Logic {

  implicit def bool2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

  def main(args: Array[String]) {
    println(table2(impl))

  }

  def not(a: Boolean) = a match {
    case true  => false
    case false => true
  }

  def equ(a: Boolean, b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(a or b)
  def nand(a: Boolean, b: Boolean): Boolean = not(a and b)
  def impl(a: Boolean, b: Boolean): Boolean = not(a) or b

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false)
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

}
