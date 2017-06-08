package advancedScala.classesAndInterfaces

trait Printable[A] {

  def format(a: A): String
}

object PrintableInstances {

  implicit val stringPrintable = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(i: Int): String = i.toString
  }

  implicit val catPrintable = new Printable[Cat] {
    override def format(a: Cat): String = {
      s"${a.name} is a ${a.age} years old ${a.color} cat."
    }
  }
}

object Printable {

  def format[A](a: A)(implicit p: Printable[A]): String = {
    p.format(a)
  }

  def print[A](a: A)(implicit p: Printable[A]): Unit = {
    println(format(a))
  }

}

object PrintableSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit p: Printable[A]): String = {
      p.format(value)
    }

    def print(implicit p: Printable[A]): Unit = {
      Printable.print(value)
    }

  }
}

object Runner {
  def main(args: Array[String]) {

    import PrintableInstances._
    import PrintableSyntax._
    val cat = Cat("lol", 1, "black")
    Printable.print(cat)
    cat.print
  }
}
