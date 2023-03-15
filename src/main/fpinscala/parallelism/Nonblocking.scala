package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

import fpinscala.parallelism.*

object Nonblocking:

  // A ⇒ Unit function is sometimes called a continuation or a callback.
  opaque type Future[+A] = (A => Unit) => Unit
  opaque type Par[+A]    = ExecutorService => Future[A]

  extension [A](pa: Par[A])
    def run(es: ExecutorService): A =
      val ref   = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      pa(es) { a =>
        ref.set(a); latch.countDown
      }
      latch.await
      ref.get

  def unit[A](a: A): Par[A] =
    es => cb => cb(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => cb => eval(es)(a(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  /* Here, a non-blocking implementation is considerably trickier. Conceptually,
  we’d like map2 to run both Par arguments in parallel. When both results have
  arrived, we want to invoke f and then pass the resulting C to the continuation.
  But there are several race conditions to worry about here, and a correct non-
  blocking implementation is difficult using only the low-level primitives of
  java.util.concurrent.
   */
  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        cb =>
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if br.isDefined then eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if ar.isDefined then eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))

    def map[B](f: A => B): Par[B] =
      es => cb => pa(es)(a => eval(es)(cb(f(a))))

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if as.isEmpty then unit(Vector())
    else if as.length == 1 then map(as.head)(a => Vector(a))
    else
      val (l, r) = as.splitAt(as.length / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))
