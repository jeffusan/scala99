package cats.caseStudy.validation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.semigroup._

object Validation {

  trait CheckF[E, A] { self ⇒
    def apply(value: A): Either[E, A]

    //    def and(that: Check[E, A])(implicit s: Semigroup[E]): Check[E, A] = {
    //      new Check[E, A] {
    //        override def apply(value: A): Either[E, A] = {
    //          self.apply(value) match {
    //            case Left(e) ⇒ that.apply(value) match {
    //              case Left(e2) ⇒ Left(e2 |+| e)
    //              case Right(_) ⇒ Left(e)
    //            }
    //            case Right(_) ⇒ that.apply(value) match {
    //              case Left(e) ⇒ Left(e)
    //              case Right(_) ⇒ Right(value)
    //            }
    //          }
    //        }
    //      }
    //    }

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] = {
      new CheckF[E, A] {
        override def apply(value: A): Either[E, A] = {
          (this(value), that(value)) match {
            case (Left(e), Left(e2)) ⇒ Left(e |+| e2)
            case (Left(e), Right(_)) ⇒ Left(e)
            case (Right(_), Left(e)) ⇒ Left(e)
            case (Right(_), Right(_)) ⇒ Right(value)
          }
        }
      }
    }


    // other methods...
  }



  import cats.syntax.validated._
  import cats.syntax.cartesian._

  sealed trait Check[E, A] {

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)

        case And(left, right) =>
          (left(a) |@| right(a)).map((_,_) ⇒ a)
      }
  }

  final case class And[E, A](
                              left: Check[E, A],
                              right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](
                               func: A => Validated[E, A]) extends Check[E, A]



}
