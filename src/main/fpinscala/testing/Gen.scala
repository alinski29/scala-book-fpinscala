package fpinscala.testing

import fpinscala.state.{State, RNG}

import Prop._

trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(that: Prop): Prop = ???

object Prop:
  opaque type SuccessCount = Int
  opaque type FailedCase   = String

  def forAll[A](a: Gen[A])(p: A => Boolean): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def listOf[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  /* Exercise 8.4
  Implement Gen.choose using this representation of Gen. It should generate
  integers in the range start to stopExclusive. Feel free to use functions you’ve already written.
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  /* Exercise 8.5
  Let’s see what else we can implement using this representation of Gen. Try
  implementing unit, boolean, and listOfN.
   */
  def unit[A](a: => A): Gen[A] = 
    // s => (a, s)
    State.unit(a)

  def boolean: Gen[Boolean] = 
    State(RNG.boolean)

  extension [A](self: Gen[A]) def listOfN(n: Int): Gen[List[A]] = 
    State.sequence(List.fill(n)(self))
