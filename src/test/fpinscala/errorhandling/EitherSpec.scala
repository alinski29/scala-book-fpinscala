package fpinscala.errorhandling

import fpinscala.TestSpec
import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Either.*

import scala.{Either as _, Right as _, Left as _}

class EitherSpec extends TestSpec {

  "Either implementation" should {

    "implement map, flatMap, orElse and map2" in {

      val left: Either[Exception, Int] = Left(new Exception("ouch"))

      Right(2).map(_ * 2) shouldEqual Right(4)
      left.map(_ * 2) shouldEqual left

      Right("foo").map(_ + "bar") shouldEqual Right("foobar")
      List(Right(2), left).map(x => x.map(_ * 2)) shouldEqual List(Right(4), left)

      left.flatMap(x => Right(x * 2)) shouldEqual left
      Right(42).flatMap(x => Right(x * 2)) shouldEqual Right(84)

      left.orElse(Right(42)) shouldEqual Right(42)
      Right(42).orElse(Right(42)) shouldEqual Right(42)

      Right(42).map2(Right(2))(_ / _) shouldEqual Right(21)
      left.map2(Right(2))(_ / _) shouldEqual left
    }

    "accumulate erros" in {
      case class Name private (value: String)
      object Name:
        def apply(name: String): Either[String, Name] =
          if name == "" || name == null then Left("Name is empty.")
          else Right(new Name(name))

      case class Age private (value: Int)
      object Age:
        def apply(age: Int): Either[String, Age] =
          if age < 0 then Left("Age is out of range.")
          else Right(new Age(age))

      case class Person(name: Name, age: Age)
      object Person:
        def makeBoth(name: String, age: Int): Either[List[String], Person] =
          map2Both(Name(name), Age(age), Person(_, _))

      val p = Person.makeBoth("", -1)
      p shouldEqual Left(List("Name is empty.", "Age is out of range."))

      val p1 = Person.makeBoth("Curry", 34)
      val p2 = Person.makeBoth("Howard", 44)

      val pair = map2All(p1, p2, (_, _))
      pair.isRight shouldBe true
    }
  }
}
