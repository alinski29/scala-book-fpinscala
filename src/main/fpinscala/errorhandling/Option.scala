package fpinscala.errorhandling

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  /* Exercise 4.1
  Implement all of the preceding functions on Option
   - def map[B](f: A => B): Option[B]
   - def flatMap[B](f: A => Option[B]): Option[B]
   - def getOrElse[B >: A](default: => B): B
   - def orElse[B >: A](ob: => Option[B]): Option[B]
   - def filter(f: A => Boolean): Option[A]
   */

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    // this match
    //   case None    => ob
    //   case Some(a) => Some(a)
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    // this match
    //   case Some(a) if f(a) => Some(a)
    //   case _               => None
    flatMap(x => if f(x) then Some(x) else None)

import Option.{Some, None}

object Option:

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  /* Exercise 4.2
    Implement the variance function in terms of flatMap. If the mean of a
    sequence is m, the variance is the mean of math.pow(x - m, 2) for each
    element x in the sequence. See the definition of variance on Wikipedia
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /* Exercise 4.3
    Write a generic function map2 that combines two Option values using a
    binary function. If either Option value is None, then the return value is too.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /* Alternative implementations
    a.flatMap(aa => b.map(bb => f(aa, bb)))

    (a, b) match
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
      case _ => None
   */

  def toIntOption(s: String): Option[Int] =
    try Some(s.toInt)
    catch case _: NumberFormatException => None

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age * 1 / 2 * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] =
    // val optAge     = toIntOption(age)
    // val optTickets = toIntOption(numberOfSpeedingTickets)
    // map2(optAge, optTickets)(insuranceRateQuote)
    for {
      a <- toIntOption(age)
      t <- toIntOption(numberOfSpeedingTickets)
    } yield insuranceRateQuote(a, t)

  /* Exercise 4.4
    Write a function sequence that combines a list of Option-s into one Option
    containing a list of all the Some values in the original list. If the original list
    contains None even once, the result of the function should be None; otherwise
    the result should be Some with a list of all the values
   */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil     => Some(Nil)
      case x :: xs => x.flatMap(h => sequence(xs).map(h :: _))

  /* Exercise 4.5
    Sometimes we’ll want to map over a list using a function that might fail, returning None if
    applying it to any element of the list returns None. For example, what if we have a whole list
    of String values that we wish to parse to Option[Int]? In that case, we can simply sequence the results of the map:
      def parseInts(as: List[String]): Option[List[Int]] = sequence(as.map(a => toIntOption(s)))
    Implement this function. It’s straightforward to do using map and sequence, but try for a more
    efficient implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil => Some(Nil)
    // case x :: xs => f(x).flatMap(h => traverse(xs)(f).map(t => h :: t))
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)

  def sequenceViaFold[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((acc, a) => acc.flatMap(aa => a.map(bb => aa :: bb)))
