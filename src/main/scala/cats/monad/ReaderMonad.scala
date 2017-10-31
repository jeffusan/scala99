package cats.monad

import cats.data.Reader
import cats.syntax.applicative._ // for `pure`

object ReaderMonad {

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db ⇒ db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String
                   ): DbReader[Boolean] = Reader(db ⇒ db.passwords.get(username).contains(password))

  def checkLogin(
                  userId: Int,
                  password: String
                ): DbReader[Boolean] = for {
    f <- findUsername(userId)
    c <- f.map(u ⇒ checkPassword(u, password)).getOrElse(false.pure[DbReader])
  } yield c
}

case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
             )
