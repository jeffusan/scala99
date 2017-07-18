package fp.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlocking {

  sealed trait Future[+A] {

    /**
      * The apply method is declared private to the fpinscala.parallelism package,
      * which means that it can only be accessed by code within that package.
      *
      * Our Future has an apply method that receives a function k that
      * expects the result of type A and uses it to perform some effect.
      * This kind of function is sometimes called a continuation or a callback.
      *
      * The apply method is marked private[parallelism] so that we don’t expose it to users of our library.
      * Marking it private[parallelism] ensures that it can only be accessed from code within the
      * parallelism package.
      * This is so that our API remains pure and we can guarantee that our laws hold.
      *
      * A => Unit? Such a function can only be useful for executing some side effect using the given A:
      * we’re making use of a common technique of using side effects as an implementation detail for a purely functional API.
      * We can get away with this because the side effects we use are not observable to code that uses Par.
      * Note that Future.apply is protected and can’t even be called by outside code.
      */
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get()
    }

    /**
      * Simply passes the value to the continuation.
      * Note that the ExecutorService isn’t needed.
      */
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: (A) => Unit): Unit = {
          cb(a)
        }
      }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit): Unit = f(k)
    }

//    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
//      map2(pa, unit(()))((a,_) => f(a))


    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

    /**
      * eval forks off evaluation of a and returns immediately.
      * The callback will be invoked asynchronously on another thread.
      */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call: Unit = r })


    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A,B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b))) //why do we need eval() here again? we've already "waited"
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))

        }
      }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))


    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => new Future[A] {
      def apply(k: A => Unit): Unit =
        n(es) { i => eval(es) {choices(i)(es)(k) }}
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(k: A => Unit): Unit = {
          cond(es) {b => eval(es) {if(b) t(es)(k) else f(es)(k)}}
        }
      }

    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => new Future[B] {
      def apply(k: (B) => Unit): Unit = {
        pa(es) ( a => choices(a)(es)(k))
      }
    }

    def join[A](a: Par[Par[A]]): Par[A] = es => new Future[A] {
      def apply(k: (A) => Unit): Unit = {
        a(es) (aa => aa(es)(k))
      }
    }




  }

}
