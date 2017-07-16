package cats.intro

// Simple JSON AST (abstract syntax tree)
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json

final case class Person(name: String, email: String)

/**
  * A type class
  * is an interface or API that represents some functionality we want to implement.
  * In Cats a type class is represented by a trait with at least one type parameter.
  * The "serialize to JSON" behavior is encoded in this trait
  */
trait JsonWriter[A] {
  def write(value: A): Json
}


/**
  * The instances
  * of a type class provide implementa ons for the types we care about,
  * including types from the Scala standard library and types from our domain model.
  */
object JsonWriterInstances {

  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }

  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }
  // etc...
}


/**
  * An interface
  * is any functionality we expose to users.
  * Interfaces to type classes are generic methods that accept instances of the type class as implicit parameters
  */

/**
  * Interface object
  */
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)


  def main(args: Array[String]) {
    import JsonWriterInstances._
    Json.toJson(Person("Dave", "dave@example.com"))
    // res4: Json = JsObject(Map(name -> JsString(Dave), email ->
    //JsString(dave@example.com)))
  }
}

/**
  * Interface Syntax
  * = extension method
  */
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  def main(args: Array[String]) {
    import JsonWriterInstances._
//    import JsonSyntax._
    Person("Dave", "dave@example.com").toJson
    // res5: Json = JsObject(Map(name -> JsString(Dave), email ->
    //JsString(dave@example.com)))
  }
}

