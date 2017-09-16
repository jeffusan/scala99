package cats.intro

import cats.Show
import cats.kernel.Eq

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


  import cats.syntax.all._
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._

  implicit val catEqual = Eq.instance[Cat] { (a ,b) =>
    a.age === b.age && a.name === b.name && a.color === b.color
  }




  def main(args: Array[String]) {

    import PrintableInstances._
    import PrintableSyntax._

    val c = Cat("bob", 1, "black")
    Printable.print(c)
    c.print

    implicit val catShow = Show.show[Cat](a => s"${a.name} is a ${a.age} yo ${a.color} cat from cats.Show.")
    import cats.syntax.show._

    println(c.show)

    val cat1 = Cat("Garfield",   35, "orange and black")
    val cat2 = Cat("Heathcliff", 30, "orange and black")
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(cat1 === cat2)
    println(cat1 =!= cat2)
    println(optionCat1 === optionCat2)
    println(optionCat1 =!= optionCat2)


  }
}