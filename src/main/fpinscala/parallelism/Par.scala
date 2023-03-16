package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit, Callable}

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  // promotes a constant value to a parallel computation
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
  extension [A](pa: Par[A])
    def run(s: ExecutorService): Future[A] =
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

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
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
    // ps.foldRight(unit(Nil: List[A]))((a, acc) => a.map2(acc)(_ :: _))
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  /* We can do a bit better in this case though, using a similar technique to the
  one we used in sums—let’s divide the computation in to two halves and compute each in parallel.
  
  Since we’ll again need efficient random access to the elements in our collection, 
  we’ll first write a version that works with an IndexedSeq[Par[A]]:
   */
  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = 
    if ps.isEmpty then unit(IndexedSeq.empty)
    else if ps.size == 1 then ps.head.map(a => IndexedSeq(a))
    else 
      val (l, r) = ps.splitAt(ps.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  /* Exercise 7.6
  Implement parFilter, which filters elements of a list in parallel.
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val pars: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(pars).map(_.flatten)
      // as.foldRight(unit(Nil: List[A]))((a, acc) => if f(a) then unit(a).map2(acc)(_ :: _) else acc)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if cond.run(es).get then t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Exerices 7.11
   Let’s say that choiceN runs n, and then uses that to select a parallel computation
   from choices. This is a bit more general than choice. Implement choiceN and then choice in terms of choiceN.
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      val idx = n.run(es).get
      choices(idx).run(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(b => if b then 0 else 1))(List(t, f))

  /* Exercise 7.12
   There’s still something rather arbitrary about choiceN. The choice of List
    seems overly specific. Why does it matter what sort of container we have?
    For instance, what if, instead of a list of computations, we have a Map of them:
   */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es =>
      val k = key.run(es).get
      choices(k).run(es)

  /* Exercise 7.13
   Implement this new primitive chooser , and then use it to implement choice and choiceN.
   */
  extension [A](pa: Par[A])
    def chooser[B](choices: A => Par[B]): Par[B] =
      es =>
        val k = pa.run(es).get
        choices(k).run(es)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i % choices.size))

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if b then t else f)

  // chooser is flatMap or bind
  extension [A](pa: Par[A])
    def flatMap[B](f: A => Par[B]): Par[B] =
      chooser(pa)(f)

  /* Exercise 7.14
  Implement join. Can you see how to implement flatMap using join? And can you implement join using flatMap?
  We call it join since conceptually it’s a parallel computation that, when run, will execute
  the inner computation, wait for it to finish (much like Thread.join), and then return its result.
   */
  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => ppa.run(es).get().run(es)

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(x => x)

  extension [A](pa: Par[A])
    def flatMapViaJoin[B](f: A => Par[B]): Par[B] =
      join(pa.map(f))
