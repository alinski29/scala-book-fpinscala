package fpinscala.parsing

import fpinscala.testing.*

import java.util.regex.*
import scala.util.matching.Regex

/* Scala’s syntax for a type parameter that is itself a type constructor. */
trait Parsers[Parser[+_]]:

  extension [A](p: Parser[A])

    def run(input: String): Either[ParseError, A]

    // Chooses between two parsers, first attempting p1, and then p2 if p1 fails
    infix def or(p2: => Parser[A]): Parser[A]

    def |(p2: => Parser[A]): Parser[A] =
      p.or(p2)

    def attempt: Parser[A]

    // Returns the portion of input inspected by p if successful
    def slice: Parser[String]

    /* Sequences two parsers, running p1 and then p2, and returns the pair of their
    results if both succeed */
    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      // p.flatMap(a => p2.map(b => (a, b)))
      for
        a <- p
        b <- p2
      yield (a, b)

    def **[B](p2: Parser[B]): Parser[(A, B)] =
      p.product(p2)

    /* Exercise 9.1
      Using product, implement the now-familiar combinator map2 and then use
      this to implement many1 in terms of many. Note that we could have chosen to
      make map2 primitive and defined product in terms of map2 as we’ve done in
      previous chapters. The choice is up to you.
     */
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      // p.product(p2).map((a, b) => f(a, b))
      for
        a <- p
        b <- p2
      yield f(a, b)

    /* Exercise 9.3 (optional)
    Hard: Before continuing, see if you can define many in terms of |, map2, and succeed.
     */
    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def opt: Parser[Option[A]] =
      p.map(Some(_)) | succeed(None)

    /* Exercise 9.4 (optional)
    Hard: Using map2 and succeed, implement the listOfN combinator from earlier.
     */
    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    /* Exercise 9.7
    Implement product and map2 in terms of flatMap.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B]

    /* Exercise 9.8
    map is no longer primitive. Express it in terms of flatMap and/or other
    combinators.
     */
    def map[B](f: A => B): Parser[B] =
      p.flatMap(a => succeed(f(a)))

    // is that if p fails, its ParseError will somehow incorporate msg.
    def label(msg: String): Parser[A]

    /* nest labels for multiple levels of error reporting.
    Unlike label, scope doesn’t throw away the label(s) attached to p—it merely
    adds additional information in the event that p fails.
     */
    def scope(msg: String): Parser[A]

    /* Sequences two parsers, ignoring the result of the first. We wrap
      the ignored half in slice, since we don't care about its result.
     */
    def *>[B](p2: => Parser[B]) =
      p.slice.map2(p2)((_, b) => b)

    /** Sequences two parsers, ignoring the result of the second. We wrap the ignored half in slice, since we don't care
      * about its result.
      */
    def <*(p2: => Parser[Any]) =
      p.map2(p2.slice)((a, b) => a)

    def +(p2: Parser[Any]) =
      (p ** p2).map((a, b) => a.toString + b.toString)

    def token: Parser[A] =
      p.attempt <* whitespace

    /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep(
        separator: Parser[Any]
    ): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
      p.sep1(separator) | succeed(Nil)

    /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    /** The root of the grammar, expects no further input following `p`. */
    def root: Parser[A] =
      p <* eof

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  // Recognizes and returns a single string
  def string(s: String): Parser[String]

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] =
    regex((".*?" + Pattern.quote(s)).r)

  // discards whitespaces at the start
  def whitespace: Parser[String] =
    regex("\\s*".r)

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def quoted: Parser[String] =
    string("\"") *> thru("\"") map (_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    quoted.label("string literal").token

  def digits: Parser[String] =
    regex("\\d+".r)

  def digits(n: Int) =
    regex(s"""\\d{$n}""".r)

  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString.map(_.toDouble).label("double literal")

  // allways succeeds with the value A, regardless of the input string
  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  //  Recognizes a regular expression r
  def regex(r: Regex): Parser[String]

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
        p.label(msg).run(input) match
          case Left(e) => e.toString == msg
          case _       => true
      }
  end Laws

end Parsers

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf("\n") match
    case -1        => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    this.copy(offset = offset + n)

  def remaining: String =
    input.substring(offset)

  def slice(n: Int) =
    input.slice(offset, offset + n)

  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next else ""
    else ""

  def columnCaret = (" " * (col - 1)) + "^"

end Location

case class ParseError(stack: List[(Location, String)] = Nil):

  def push(loc: Location, msg: String): ParseError =
    this.copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    this.stack.lastOption

  def latestLoc: Option[Location] =
    latest.map(_(0))

  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  def formatLoc(l: Location): String =
    s"${l.line}.${l.col}"

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1)
      .view
      .mapValues(_.map(_._2).mkString("; "))
      .toList
      .sortBy(_._1.offset)

end ParseError
