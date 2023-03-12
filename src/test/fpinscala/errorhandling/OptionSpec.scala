package fpinscala.errorhandling

import fpinscala.TestSpec
import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Option.*

class OptionSpec extends TestSpec {

  "Option implementation" should {

    "implement map, flatMap, getOrElse, orElse and filter" in {

      Some(2).map(_ * 2) shouldEqual Some(4)
      (None: Option[Int]).map(_ * 2) shouldEqual None
      Some("foo").map(_ + "bar") shouldEqual Some("foobar")
      List(Some(2), None).map(x => x.map(_ * 2)) shouldEqual List(Some(4), None)

      None.getOrElse(42) shouldEqual 42
      Some(42).getOrElse(43) shouldEqual 42

      (None: Option[Int]).flatMap(x => Some(x * 2)) shouldEqual None
      Some(42).flatMap(x => Some(x * 2)) shouldEqual Some(84)

      None.orElse(Some(42)) shouldEqual Some(42)
      Some(42).orElse(Some(43)) shouldEqual Some(42)

      Some(42).filter(_ % 2 == 0) shouldEqual Some(42)
      Some(43).filter(_ % 2 == 0) shouldEqual None
      (None: Option[Int]).filter(_ % 2 == 0) shouldEqual None
    }

    "implement variance in terms of flatMap" in {
      val nums = Seq(10, 12, 14, 8).map(_.toDouble)
      mean(nums) shouldEqual Some(11.0)
      variance(nums) shouldEqual Some(5.0)
    }

    "combine 2 Option values using a binary function" in {
      map2(Some(42), Some(2))(_ / _) shouldEqual Some(21)
      map2(None: Option[Int], Some(2))(_ / _) shouldEqual None

      parseInsuranceRateQuote("18", "100") shouldEqual Some(900)
      parseInsuranceRateQuote("foo", "bar") shouldEqual None
    }

    "combine list of Options into 1 Option using sequence" in {
      sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
      sequence(List(Some(1), Some(2), None)) shouldEqual None
      sequence(List(None, None)) shouldEqual None

      sequenceViaTraverse(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
      sequenceViaTraverse(List(Some(1), Some(2), None)) shouldEqual None
      sequenceViaTraverse(List(None, None)) shouldEqual None

      sequenceViaFold(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
      sequenceViaFold(List(Some(1), Some(2), None)) shouldEqual None
      sequenceViaFold(List(None, None)) shouldEqual None
    }
  }
}
