package fpinscala.errorhandling

import scala.util.control.NonFatal
import scala.{Either as _, Right as _, Left as _}

// We can say that it’s a disjoint union of two types (union ADT)
enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  /* Exercise 4.6
    Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
   */
  def map[B](f: A => B): Either[E, B] = this match
    case Left(v)  => Left(v)
    case Right(v) => Right(f(v))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(v)  => Left(v)
    case Right(v) => f(v)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(v)  => b
    case Right(v) => Right(v)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a  <- this
      b1 <- b
    yield f(a, b1)
    // this.flatMap(a => b.map(bb => f(a, bb)))

  def isRight = this match
    case Right(v) => true
    case _        => false

object Either:

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  /* We can extract a more general function catchNonFatal, which factors out
  this common pattern of converting thrown exceptions to value */
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  /* Exercise 4.7
    Implement sequence and traverse for Either. These should return
    the first error that’s encountered, if there is one.
   */
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = as match
    case Nil     => Right(Nil)
    case x :: xs => x.flatMap(h => sequence(xs).map(h :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match
    case Nil     => Right(Nil)
    case x :: xs => f(x).flatMap(h => traverse(xs)(f).map(t => h :: t))

  def map2Both[E, A, B, C](
      a: Either[E, A],
      b: Either[E, B],
      f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_))    => Left(List(e))
      case (Right(_), Left(e))    => Left(List(e))
      case (Left(e1), Left(e2))   => Left(List(e1, e2))

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_))   => Left(es)
      case (Right(_), Left(es))   => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)

  import fpinscala.errorhandling.Option.insuranceRateQuote

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Throwable, Double] =
    for {
      a       <- Either.catchNonFatal(age.toInt)
      tickets <- Either.catchNonFatal(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)
