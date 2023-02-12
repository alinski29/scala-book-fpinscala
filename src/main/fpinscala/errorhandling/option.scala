package fpinscala.errorhandling

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

// object Option:
