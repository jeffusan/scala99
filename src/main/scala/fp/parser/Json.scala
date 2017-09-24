package fp.parser

import language.higherKinds

trait Json

object Json {
  case object JNull extends Json
  case class JNumber(get: Double) extends Json
  case class JString(get: String) extends Json
  case class JBool(get: Boolean) extends Json
  case class JArray(get: IndexedSeq[Json]) extends Json
  case class JObject(get: Map[String, Json]) extends Json

//  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[Json] = {
//    import P._
//    val spaces = char(' ').many.slice ...
//  }
}
