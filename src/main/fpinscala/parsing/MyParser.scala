package fpinscala.parsing

import scala.util.matching.Regex

enum Result[+A]:
  case Success(get: A, charsConsumed: Int)
  case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def extract: Either[ParseError, A] = this match
    case Failure(e, _) => Left(e)
    case Success(a, _) => Right(a)

  def mapError(f: ParseError => ParseError): Result[A] =
    this match
      case Failure(e, c) => Failure(f(e), c)
      case _             => this

  def uncommit: Result[A] =
    this match
      case Failure(e, true) => Failure(e, false)
      case _                => this

  def addCommit(isCommitted: Boolean): Result[A] =
    this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this

  def advanceSuccess(n: Int): Result[A] =
    this match
      case Success(a, m) => Success(a, n + m)
      case _             => this

end Result

object MyParser extends Parsers[MyParser.Parser]:
  import Result.{Success, Failure}

  /* A parser is a kind of state action that can fail. */
  type Parser[+A] = Location => Result[A]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the first index where the two strings differed. If s2 is longer
    * than s1, returns s1.length.
    */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  /* Exercise 9.13
    Implement string, regex, succeed, and slice for this initial representation
    of Parser. Note that slice is less efficient than it could be, since it must still
    construct a value only to discard it. We’ll return to this later.
   */
  // def string(s: String): Parser[String] =
  //   loc =>
  //     if (loc.input.startsWith(s, loc.offset)) then Success(s, s.length)
  //     else Failure(loc.toError(s"Expected $s"), false)

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then // they matched
        Success(w, w.length)
      else Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  def regex(r: Regex): Parser[String] =
    loc =>
      r.findPrefixOf(loc.remaining) match
        case Some(m) => Success(m, m.length)
        case _       => Failure(loc.toError(s"Expected pattern ${r.toString}"), false)

  // // consume no characters and succeed with the given value
  def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  def fail(msg: String): Parser[Nothing] =
    loc => Failure(loc.toError(msg), true)

  def _run[A](p: Parser[A], input: String): Either[ParseError, A] =
    p(Location(input, 0)).extract

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A] =
      _run(p, input)

  extension [A](p: Parser[A])

    def slice: Parser[String] =
      loc =>
        p(loc) match
          case Success(_, n)     => Success(loc.slice(n), n)
          case f @ Failure(_, _) => f

    def scope(msg: String): Parser[A] =
      loc => p(loc).mapError(_.push(loc, msg))

    def label(msg: String): Parser[A] =
      loc => p(loc).mapError(_.label(msg))

    def attempt: Parser[A] =
      loc => p(loc).uncommit

    def or(p2: => Parser[A]): Parser[A] =
      loc =>
        p(loc) match
          case Failure(e, false) => p2(loc)
          case r                 => r

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l =>
        // println(s"l: $l; p(l): ${p(l)}")
        p(l) match
          case Success(a, n)     => f(a)(l.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
          case f @ Failure(_, _) => f

    /* We provide an overridden version of `many` that accumulates
     * the list of results using a monolithic loop. This avoids
     * stack overflow errors for most grammars.
     */
    override def many: Parser[List[A]] =
      l =>
        var nConsumed: Int = 0
        val buf            = new collection.mutable.ListBuffer[A]
        def go(p: Parser[A], offset: Int): Result[List[A]] =
          p(l.advanceBy(offset)) match
            case Success(a, n) =>
              buf += a
              go(p, offset + n)
            case Failure(e, true) => Failure(e, true)
            case Failure(_, _)    => Success(buf.toList, offset)
        go(p, 0)

end MyParser
