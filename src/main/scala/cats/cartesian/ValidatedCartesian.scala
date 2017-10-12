package cats.cartesian

object ValidatedCartesian {

  import cats.data.Validated

  type FormData = Map[String, String]
  type ErrorsOr[A] = Either[List[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(name: String)(data: FormData): ErrorsOr[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  val getName = getValue("name") _
  // getName: FormData => ErrorsOr[String] = <function1>

  getName(Map("name" -> "Dade Murphy"))
  // res25: ErrorsOr[String] = Right(Dade Murphy)

//  def nonBlank2(e: String): ErrorsOr[String] = if(e.isEmpty) Left(List("field is empty")) else Right(e)


//    if (i >= 0) Right(i) else Left(List(s"$i is negative"))


  import cats.syntax.either._

  def nonNegative(name: String)(i: Int): ErrorsOr[Int] =
    Right(i).ensure(List(s"$i is negative"))(_ >= 0)

  def parseInt(name: String)(data: String): ErrorsOr[Int] =
    Right(data).
      flatMap(s => Either.catchOnly[NumberFormatException](s.toInt)).
      leftMap(_ => List(s"$name must be an integer"))

  def nonBlank(name: String)(data: String): ErrorsOr[String] =
    Right(data).
      ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def readName(data: FormData): ErrorsOr[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): ErrorsOr[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

}
