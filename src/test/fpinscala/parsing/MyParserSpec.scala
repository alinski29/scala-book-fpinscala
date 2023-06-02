package fpinscala.parsing

import fpinscala.TestSpec
import fpinscala.parsing.*
import java.util.regex.Pattern

class MyParserSpec extends TestSpec {

  "MyParser" should {

    import MyParserSpec.*
    import fpinscala.parsing.MyParser.{run, slice}

    val parser = new Examples(MyParser)

    "parse non negative integers" in {
      parser.nonNegativeInt.run("123") shouldEqual Right(123)
      parser.nonNegativeInt.run("foo").isLeft shouldBe true
      parser.nonNegativeInt.run("-10").isLeft shouldBe true
    }

    "parse a character many times" in {
      parser.aMany.run("aaa") shouldEqual Right(List("a", "a", "a"))
      parser.aMany.run("abb") shouldEqual Right(List("a"))
      parser.aMany.run("bab") shouldEqual Right(Nil)
      parser.aMany.slice.run("aaa") shouldEqual Right("aaa")

      parser.a3Parser.run("aa").isLeft shouldBe true
      parser.a3Parser.run("aaa") shouldEqual Right("aaa")
    }

    "parse a sequence or another" in {
      parser.aorb.run("abbaabcb") shouldEqual Right("abbaab")
      parser.aorb.run("cab") shouldEqual Right("")
    }

    "parse quoted strings" in {
      parser.quoted.run("\"foo\"") shouldEqual Right("foo")
      parser.quoted.run("foo").isLeft shouldBe true
      parser.quoted.run("\"foo").isLeft shouldBe true
    }

    "parse tokens and ignore whitespaces" in {
      parser.tkn("->").run("-> bar") shouldEqual Right("->")
      parser.tkn("->").run(": bar").isLeft shouldBe true
    }

    "parse phone numbers" in {
      parser.phone.run("+40 729 120 999").isRight shouldBe true
      parser.phone.run("+40729120999").isRight shouldBe true
      // parser.phone.run("+4072912099999").isRight shouldBe false
    }

  }
}

object MyParserSpec:

  case class PhoneNumber(countryCode: Int, Number: String)

  class Examples[Parser[+_]](P: Parsers[Parser]):
    import P.*

    val nonNegativeInt: Parser[Int] =
      for
        nString <- regex("[0-9]+".r)
        n <- nString.toIntOption match
          case Some(n) => succeed(n)
          case None    => fail("expected an integer")
      yield n

    val aMany = string("a").many

    val a3Parser = char('a').listOfN(3).slice

    val aorb = (char('a') | char('b')).many.slice

    val quoted = (string("\"") *> thru("\"")).map(_.dropRight(1))

    def tkn(s: String) = string(s).token

    val countryCode = string("+").label("+") *> digits(2).label("country code digits")

    val number = regex("[0-9 ]+".r).map(_.filterNot(_.isWhitespace))

    val phone = (countryCode ** number <* eof).map { case (code, no) => PhoneNumber(code.toInt, no) }
