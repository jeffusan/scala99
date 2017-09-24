package fp.parser

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  //Parser[Int] that recognizes zero or more 'a' characters, and whose result value is the number of 'a' characters it has seen.
  // For instance, given "aa", the parser results in 2;
  // given "" or "b123" (a string not starting with 'a'), it results in 0; and so on.

//  "basic parser" should "verify laws" in {
  //    val testParser = new Parser[String, String]() {
  //      override def run[A](p: Parser[A])(input: String): Either[String, A] = ???
  //
  //      override def char(c: Char): Parser[Char] = ???
  //
  //      override def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
  //
  //      override implicit def string(s: String): Parser[String] = ???
  //
  //      override def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???
  //    }
  //
  //    testParser.ru
  //  }



}
