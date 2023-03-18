package fpinscala.testing

import fpinscala.state.{State, RNG}
import scala.util.{Try, Success, Failure}

import Prop._

opaque type Prop = (MaxSize, TestCases, RNG) => Result
object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount          = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases          = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase        = s

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize          = x

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed          => false
      case Falsified(_, _) => true

  extension (self: Prop)
    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

  extension (self: Prop)
    def run(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)
    ): Unit =
      self(maxSize, testCases, rng) match
        case Result.Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests for value: $msg")
        case Result.Passed => 
          println(s"+ OK, passed $testCases tests.")

  def forAll[A](as: Gen[A])(p: A => Boolean): Prop =
    @annotation.tailrec
    def viaRec(n: Int, rng: RNG): Result =
      if n == 0 then Result.Passed
      else
        val (aVal, rngNxt) = as.run(rng)
        Try(p(aVal)) match
          case Success(false)        => Result.Falsified(buildMsg(aVal, new Exception(s"Case failed: $aVal")), n)
          case Failure(e: Exception) => Result.Falsified(buildMsg(aVal, e), n)
          case _                     => viaRec(n - 1, rngNxt)

    def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(g.run(rng)))

    def viaLazyList(n: Int, rng: RNG) =
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map { case (a, i) =>
          // println(s"a: $a, i: $i")
          try if p(a) then Result.Passed else Result.Falsified(a.toString, i)
          catch case e: Exception => Result.Falsified(buildMsg(a, e), i)
        }
        .find(_.isFalsified)
        .getOrElse(Result.Passed)

    (_, n, rng) =>
      // viaRec(n, rng)
      viaLazyList(n, rng)

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  @annotation.targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] = LazyList
        .from(0)
        .take((n.toInt min max.toInt) + 1)
        .map(i => forAll(g(i))(f))
      val prop: Prop = props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  /* Exercise 8.9
  Now that we have a representation of Prop, implement && and || for
  composing Prop values. Notice that in the case of failure we don’t know
  which property was responsible, the left or the right. Can you devise a way of
  handling this, perhaps by allowing Prop values to be assigned a tag or label
  which gets displayed in the event of a failure?
   */
  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("and-left")(max, n, rng) match
          case Result.Passed => that.tag("and-right")(max, n, rng)
          case x             => x

  extension (self: Prop)
    def ||(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("or-left")(max, n, rng) match
          case Result.Falsified(msg, _) => that.tag("or-right").tag(msg)(max, n, rng)
          case x                        => x

  extension (self: Prop)
    def tag(msg: String): Prop =
      (max, n, rng) =>
        self(max, n, rng) match
          case Result.Falsified(e, c) => Result.Falsified(s"$msg($e)", c)
          case Result.Passed          => Result.Passed
end Prop

opaque type Gen[+A] = State[RNG, A]
object Gen:
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

    /* Exercise 8.6
    Implement flatMap, and then use it to implement this more dynamic version
    of listOfN. Put flatMap and listOfN in the Gen class.
     */
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)
  
    /* Exercise 8.10
    Implement a helper function for converting a Gen to an SGen which ignores
    the size parameter. You can add this as an extension method on Gen.
    */
    def unsized: SGen[A] =
      _ => self

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)
    
    /* Exercise 8.12
    Implement a list combinator that doesn’t accept an explicit size. It should
    return an SGen instead of a Gen. The implementation should generate lists of
    the requested size.
     */
    def list: SGen[List[A]] =
      n => listOfN(n)



  /* Exercise 8.4
  Implement Gen.choose using this representation of Gen. It should generate
  integers in the range start to stopExclusive. Feel free to use functions you’ve already written.
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  /* Exercise 8.5
  Let’s see what else we can implement using this representation of Gen. Try implementing unit,
  boolean, and listOfN.
   */
  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG.boolean)

  /* Exercise 8.7
  Implement union, for combining two generators of the same type into one, by
  pulling values from each generator with equal likelihood.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(bool => if bool then g1 else g2)
    // for
    //   bool <- Gen.boolean
    //   gen  <- if bool then g1 else g2
    // yield gen
    //

  /* Exercise 8.8 (optional)
    Implement weighted, a version of union that accepts a weight for each Gen
    and generates values from each Gen with probability proportional to its weight
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

end Gen

opaque type SGen[+A] = Int => Gen[A]
object SGen:
  /* Exercise 8.11 (optional)
  Not surprisingly, SGen at a minimum supports many of the same operations as Gen,
  and the implementations are rather mechanical. Define some convenience functions
  on SGen that simply delegate to the corresponding functions on Gen.[86]
   */
  extension [A](self: SGen[A])

    def apply(n: Int): Gen[A] =
      self(n)

    def map[B](f: A => B): SGen[B] =
      self(_).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(f(_)(n))

    // def **[B](s2: SGen[B]): SGen[(A, B)] =
    //   n => Gen.**(apply(n))(s2(n))

  def apply[A](f: Int => Gen[A]): SGen[A] = f

end SGen
