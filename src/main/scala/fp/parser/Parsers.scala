package fp.parser

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex


trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]

  /**
    * But slice does put a constraint on the implementation, namely,
    * that even if the parser p.many.map(_.size) will generate an intermediate list when run,
    * slice(p.many).map(_.size) will not. This is a strong hint that slice is primi- tive,
    * since it will have to have access to the internal representation of the parser.
    *
    * Returns the portion of input inspected by p if successful
    */
  def slice[A](p: Parser[A]): Parser[String]

  /**
    *  Chooses between two parsers, first attempting p1, and then p2 if p1 fails
    */
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  /**
    * Runs a parser, then uses its result to select a second parser to run in sequence
    */
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
    * Always succeeds with the value a
    */
  def succeed[A](a: A): Parser[A]

  /**
    * Recognizes a regular expression s
    */
  implicit def regex(r: Regex): Parser[String]

  /**
    * Recognizes and returns a single String
    */
  implicit def string(s: String): Parser[String]


  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  implicit def asCharParser(c: Char): Parser[Char] = char(c)


  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
  run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if(n <= 0) succeed(List[A]())
    else map2(p, listOfN(n - 1, p))(_ :: _) or succeed(List[A]())
  }

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")


  /**
    *  tries running p,
    *  followed by many(p) again, and again, and so on until the attempt to parse p fails.
    *  It’ll accumulate the results of all successful runs of p into a list.
    *  As soon as p fails, the parser returns the empty List.
    *  or, map2, and succeed
    */
  def many0[A](p: Parser[A]): Parser[List[A]] = {
    val a = p.map(List(_)).or(succeed(List[A]()))
    map2(p, a)((a,b) => a :: b)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or succeed(List[A]())
  }

  def zeroOrMore[A](p: Parser[A]): Parser[Int] = map(many(p))(_.size)
  def oneOrMore[A](p: Parser[A], message: String): Parser[List[A]]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(f andThen succeed)
  }

  def char2(c: Char): Parser[Char] = string(c.toString) map(_.charAt(0))

  run(slice((char('a')|char('b')).many))("aaba")

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, p.many){(a,b) => a :: b}
  }

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
    p.flatMap(a => p2.map(b => (a,b)))

    for {
      a <- p
      b <- p2
    } yield (a,b)
  }

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    product(p,p2).map(ab => f(ab._1, ab._2))
  }

  def map2ViaFlatmap[A,B, C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    for {
      a <- p
      b <- p2
    } yield f(a,b)
  }

  def digitString[A](p: Parser[A]): Parser[String] = {
    "\\d".r flatMap(many(_).slice)
  }

  /**
    * parser for zero or more 'a' followed by one or more 'b'
    */
  char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    val numA: Parser[Int] = char('a').many.map(_.size)
    def product[B](b: Parser[B]):Parser[(A,B)] = self.product(p, b)
    def **[B](b: Parser[B]): Parser[(A,B)] = product(b)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

}