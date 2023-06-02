package fpinscala.monads

import fpinscala.testing.Gen
import fpinscala.parallelism.*
import fpinscala.parsing.*
import fpinscala.state.*

/* LAWS
 * identity:
 *  - compose(f, unit) == f
 *  - compose(unit, f) == f
 *  associativity
 *  - combine(combine(x, y), z) == combine(x, combine(y, z))
 *  - or x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
 *
 * A monad is an implementation of one of the minimal sets of monadic
 * combinators, satisfying the laws of associativity and identity.
 */
trait Monad[F[_]]:

  def unit[A](a: => A): F[A]
  /* Exercise 11.3
    The sequence and traverse combinators should be pretty familiar to you by
    now, and your implementations of them from various prior chapters are
    probably all very similar. Implement them once and for all on Monad[F].
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  /* Exercise 11.4 */
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  /* Exericse 11.6 */
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List.empty[A])) { (a, acc) =>
      f(a).flatMap(b => if b then unit(a).map2(acc)(_ :: _) else acc)
    }

  def replicateNViaRecursion[A](n: Int, fa: F[A]): F[List[A]] =
    if n == 0 then unit(List.empty[A])
    else fa.map2(replicateNViaRecursion(n - 1, fa))(_ :: _)

  /* Exercise 11.7
  Implement the Kleisli composition function compose.
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  // asoociative law for monads:
  // compose(compose(f, g), h) == compose(f, compose(g, h))

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    /* Exercise 11.8
    Hard: Implement flatMap in terms of compose. It seems that we’ve found
    another minimal set of monad combinators: compose and unit.
     */
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      compose(a => fa, f)(())

  /* Exercise 11.12
  There’s a third minimal set of monadic combinators: map, unit, and join.
  Implement join in terms of flatMap.
   */
  def join[A](ffa: F[F[A]]): F[A] =
    ffa.flatMap(identity)

  /* Exercise 11.13
  Implement either flatMap or compose in terms of join and map.
   */
  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      join(fa.map(f))

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(f(a).map(b => g(b)))

object Monad:

  given genMonad: Monad[Gen] with {
    def unit[A](a: => A): Gen[A] =
      Gen.unit(a)

    extension [A](fa: Gen[A])
      def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)
  }

  /* Exercise 11.1
  Write monad instances for Option, List, LazyList, Par, and Parser.
   */
  given optionMonad: Monad[Option] with {
    def unit[A](a: => A): Option[A] =
      Some(a)

    extension [A](fa: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] =
        fa.flatMap(f)
  }

  given listMonad: Monad[List] with {
    def unit[A](a: => A): List[A] =
      List(a)

    extension [A](xs: List[A])
      def flatMap[B](f: A => List[B]): List[B] =
        xs.flatMap(f)
  }

  given lazyListMonad: Monad[LazyList] with {
    def unit[A](a: => A): LazyList[A] =
      LazyList(a)

    extension [A](xs: LazyList[A])
      def flatMap[B](f: A => LazyList[B]): LazyList[B] =
        xs.flatMap(f)
  }

  given parMonad: Monad[Par] with {
    def unit[A](a: => A): Par[A] =
      Par.unit(a)

    extension [A](fa: Par[A])
      def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] =
    new {
      def unit[A](a: => A): P[A] = p.succeed(a)

      extension [A](fa: P[A])
        def flatMap[B](f: A => P[B]): P[B] =
          p.flatMap(fa)(f)
    }

  /* We could say that monads provide a context for introducing
  and binding variables, and performing variable substitution. */

  type IntState[A] = State[Int, A]

  given intStateMonad: Monad[IntState] with {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))
    extension [A](st: IntState[A])
      override def flatMap[B](f: A => IntState[B]): IntState[B] =
        State.flatMap(st)(f)
  }

  /* An anonymous type constructor declared inline like this is called a type
  lambda in Scala. We can use this feature to partially apply the State type
  constructor and declare a monad instance for any state type S: */
  given stateMonad[S]: Monad[[x] =>> State[S, x]] with {
    override def unit[A](a: => A): State[S, A] =
      State(s => (a, s))
    extension [A](st: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
  }

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) =>
      for
        xs <- acc
        n  <- State.get
        _  <- State.set(n + 1)
      yield (n, a) :: xs
    ).run(0)
      ._1
      .reverse
