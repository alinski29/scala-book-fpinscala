package fpinscala.errorhandling

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???

import Option.{Some, None}

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  // a.flatMap(aa => b.map(bb => f(aa, bb)))
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

def toIntOption(s: String): Option[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] =
  val optAge     = toIntOption(age)
  val optTickets = toIntOption(numberOfSpeedingTickets)
  map2(optAge, optTickets)(insuranceRateQuote)
// for {
//   a <- toIntOption(age)
//   t <- toIntOption(numberOfSpeedingTickets)
// } yield insuranceRateQuote(a,t)

/* Ex 4.4 Write a function sequence that combines a list of Option-s into one Option
containing a list of all the Some values in the original list. If the original list
contains None even once, the result of the function should be None; otherwise
the result should be Some with a list of all the values
 */
def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
  case Nil     => Some(Nil)
  case x :: xs => x.flatMap(h => sequence(xs).map(h :: _))

def sequenceViaFold[A](as: List[Option[A]]): Option[List[A]] =
  as.foldLeft[Option[List[A]]](Some(Nil))((b, a) => a.flatMap(aa => b.map(bb => aa :: bb)))

/* 
 * Ex 4.5: Sometimes we’ll want to map over a list using a function that might fail,
returning None if applying it to any element of the list returns None. For
example, what if we have a whole list of String values that we wish to parse
to Option[Int]? In that case, we can simply sequence the results of the map:
def parseInts(as: List[String]): Option[List[Int]] = sequence(as.map(a => toIntOption(s)))
 * Implement this function. It’s straightforward to do using map and sequence,
but try for a more efficient implementation that only looks at the list once. In
fact, implement sequence in terms of traverse.
 */
def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match 
  case Nil => Some(Nil)
  case x :: xs => f(x).flatMap(h => traverse(xs)(f).map(t => h :: t))
  // case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)


