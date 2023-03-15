package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit, Callable}

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  /* promotes a constant value to a parallel computation  */
  def unit[A](a: A): Par[A] = 
    es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone                              = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled                         = false
    def cancel(evenIfRunning: Boolean)      = false

  // wrapes its unevalauted argument in a Par and marks it for a concurrent evaluation
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // explicitly state the given Par should run in a separate logical thread.
  // the computation won't occur until forced by run
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { def call = a(es).get })
  
  /* Exercise 7.4
    Using lazyUnit, write a function to convert any function A => B to one that
    evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // run extracts a value from a Par by actually performing the computation.
  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = 
    pa(s)
    // UnitFuture(pa(s).get())

  /* Exercise 7.1
    Par.map2 is a new higher-order function for combining the result of two
    parallel computations. What is its signature? Give the most general signature
    possible (don’t assume it works only for Int)
   */
  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

  /* Exercise 7.3 (optional)
    Hard: Fix the implementation of map2 so that it respects the contract of
    timeouts on Future.
   */
  extension [A](pa: Par[A])
    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C]:
          private val futureA                    = pa(es)
          private val futureB                    = pb(es)
          @volatile private var cache: Option[C] = None

          def isDone = cache.isDefined
          def get()  = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, units: TimeUnit): C =
            val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
            val started      = System.nanoTime
            val a            = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
            val elapsed      = System.nanoTime - started
            val b            = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
            val c            = f(a, b)
            cache = Some(c)
            c

          def isCancelled = futureA.isCancelled || futureB.isCancelled
          def cancel(evenIfRunning: Boolean) =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = 
    // parList.map2(unit(()))((a, _) => a.sorted)
    parList.map(_.sorted)

  extension [A](pa: Par[A]) def map[B](f: A => B): Par[B] = 
    pa.map2(unit(()))((a, _) => f(a))

  // if we don't wrap sequence inside fork, there will be some computation on the running thread, the sequence transformation.
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = 
    fork {
      sequence(ps.map(asyncF(f)))
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get
    
  /* Exercise 7.5
  Write this function, called sequence. No additional primitives are required.
  Do not call run.
  */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight(unit(Nil: List[A]))((a, acc) => a.map2(acc)(_ :: _))

  /* Exercise 7.6
  Implement parFilter, which filters elements of a list in parallel.
  */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = 
    fork {
      as.foldRight(unit(Nil: List[A]))((a, acc) =>
        if f(a) then unit(a).map2(acc)(_ :: _) else acc)
    }


