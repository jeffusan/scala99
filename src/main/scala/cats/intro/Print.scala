package cats.intro

import cats.Show

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val intPrintable = new Printable[Int] {
    def format(a: Int): String = {
      a.toString
    }
  }

  implicit val stringPrintable = new Printable[String] {
    def format(a: String): String = a
  }

  implicit val catPrintable = new Printable[Cat] {
    def format(a: Cat): String = {
      s"${a.name} is a ${a.age} yo ${a.color} cat."
    }
  }
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(p.format(a))
}

final case class Cat(name: String,
                     age: Int,
                     color: String)

object PrintableSyntax {
  implicit class PrintOps[A](a: A) {
    def format(implicit p: Printable[A]): String = p.format(a)

    def print(implicit p: Printable[A]): Unit = println(p.format(a))
  }
}



object Print {
  def main(args: Array[String]) {

    import PrintableInstances._
    import PrintableSyntax._

    val c = Cat("bob", 1, "black")
    Printable.print(c)
    c.print

    implicit val catShow = Show.show[Cat](a => s"${a.name} is a ${a.age} yo ${a.color} cat from cats.Show.")
    import cats.syntax.show._

    println(c.show)


  }
}