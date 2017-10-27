package cats.caseStudy.asynchronousTesting

object AsyncTesting {

  def testTotalUptime(): Unit = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

}

import cats.{Applicative, Id}

import scala.concurrent.Future
import language.higherKinds

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.functor._

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id[Int]] {
  def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}

class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future[Int]] {
  def getUptime(hostname: String): Future[Int] = Future.successful(hosts.getOrElse(hostname, 0))
}
