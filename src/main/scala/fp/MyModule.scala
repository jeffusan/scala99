package fp

object MyModule {


  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n -1 , acc * n)
    }
    go(n, 1)
  }

  def findFirst0[A](in: Array[A], key: A): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= in.length) -1
      else if (in(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](in: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= in.length) -1
      else if (p(in(n))) n
      else loop(n + 1)
    }
    loop(0)
  }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }




  def main(args: Array[String]) {

        //    def compare(i: Int, j: Int): Boolean = i<j
        //    println(isSorted(Array(1,2,3), compare))
        //    println(isSorted(Array(1,4,3), compare))
    
//        println(findFirst(Array(1,2,3), (x: Int) => x == 2))

//        val res = List(1,2,3,4,5) match {
//          case Cons(x, Cons(2, Cons(4, _))) => x
//          case Nil => 42
//          case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//          case Cons(h, t) => h + List.sum(t)
//          case _ => 101
//        }
//
//    println(res)

//        println(findFirst(Array(1,2,3), (x: Int) => x == 2))

//    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    println(List.length(List(1,2,3)))
  }

}
