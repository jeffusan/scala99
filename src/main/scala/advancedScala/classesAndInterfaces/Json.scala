package advancedScala.classesAndInterfaces

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {

  implicit val stringJsonWriter = new JsonWriter[String] {
    override def write(value: String): Json = {
      JsString(value)
    }
  }

  implicit val personJsonWriter = new JsonWriter[Person] {
    override def write(value: Person): Json = {
      JsObject(
        Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        )
      )
    }
  }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

object Other {

  def main(args: Array[String]) {
    import JsonWriterInstances._
    import JsonSyntax._

    println(Json.toJson(Person("Dave", "dave@example.com")))
  }
}