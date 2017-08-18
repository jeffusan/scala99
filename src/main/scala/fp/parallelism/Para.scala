package fp.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.Duration

object Para {

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }


  //  def sum2(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1)
  //      ints.headOption getOrElse 0
  //    else {
  //      val (l,r) = ints.splitAt(ints.length/2)
  //      val sumL: Par[Int] = Par.unit(sum(l))
  //      val sumR: Par[Int] = Par.unit(sum(r))
  //      Par.get(sumL) + Par.get(sumR)
  //    }
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class AFuture[A](get: A) extends Future[A] {
    var done = false
    var cancelled = false
    var started = false

    def isDone: Boolean = done

    def get(timeout: Long, units: TimeUnit): A = {
      started = true
      val t0 = System.currentTimeMillis()
      val d = Duration(timeout, units).toMillis
      val limit = t0 + d
      val r = get
      if(System.currentTimeMillis() > limit || cancelled) {
        throw new InterruptedException
      } else {
        done = true
        r
      }
    }

    def isCancelled: Boolean = cancelled

    def cancel(evenIfRunning: Boolean): Boolean = {
      if (evenIfRunning || !started)
        cancelled = true
      cancelled
    }

  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    map(seqWithIndexedSeq(fbs.toVector))(_.toList)
  }

  def sequenceSeq[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(List[A]())
      case h :: t => map2(h, sequenceSeq(t))(_ :: _)
    }
  }

  def sequenceViaFold[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(unit(List[A]()))((r, e) => map2(e, r)(_ :: _))
  }

  def sequenceBalanced[A](ps: List[Par[A]]): Par[List[A]] = fork {
    if(ps.isEmpty){
      unit(List())
    } else if(ps.length == 1) {
      map(ps.head)(List(_))
    } else {
      val (l1, l2) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l1), sequenceBalanced(l2))(_ ::: _)
    }
  }

  def seqWithIndexedSeq[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if(as.isEmpty){
      unit(Vector())
    } else if(as.length == 1) {
      map(as.head)(Vector(_))
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(seqWithIndexedSeq(l), seqWithIndexedSeq(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      map(seqWithIndexedSeq(ps.toIndexedSeq))(_.toList)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val vectorPar = as.map(asyncF(a => if(f(a)) List(a) else List())).toVector
    map(seqWithIndexedSeq(vectorPar))(_.toList.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

}

