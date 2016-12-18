object TestForeachOption {

  def main(args: Array[String]) {
    Fizz(None).target.foreach(println(_))
    Fizz(Some("test")).target.foreach(println(_))
  }

}

case class Fizz ( target: Option[String])
