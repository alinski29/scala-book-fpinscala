package fpinscala.laziness

import fpinscala.TestSpec
import fpinscala.laziness.LazyList
import fpinscala.laziness.LazyList.*

class LazyListSpec extends TestSpec {

  "LazyList implementation" should {
    val ll = LazyList((0 to 5): _*)

    "take elements" in {
      Empty.take(1).toList shouldEqual Nil
      ll.take(3).toList shouldEqual List(0, 1, 2)
      ll.take(10).toList shouldEqual ll.toList
    }

    "drop elements" in {
      Empty.drop(1).toList shouldEqual Nil
      ll.drop(3).toList shouldEqual List(3, 4, 5)
      ll.drop(10).toList shouldEqual Nil
    }

    "takeWhile" in {
      LazyList[Int]().takeWhile(_ % 2 == 0).toList shouldEqual Nil
      ll.takeWhile(_ < 3).toList shouldEqual List(0, 1, 2)
      ll.takeWhileFoldRight(_ < 3).toList shouldEqual List(0, 1, 2)
      ll.takeWhile(_ > 10).toList shouldEqual Nil
    }

    "exists" in {
      ll.exists(_ % 2 == 0) shouldBe true
    }

    "headOption" in {
      ll.headOption shouldEqual Some(0)
      ll.headOptionFoldRight shouldEqual Some(0)
      LazyList().headOption shouldEqual None
      LazyList().headOptionFoldRight shouldEqual None
    }

    "map, flatmap & filter" in {
      ll.map(_ * 2).toList shouldEqual ll.toList.map(_ * 2)
      ll.filter(_ % 2 == 0).toList shouldEqual ll.toList.filter(_ % 2 == 0)
      ll.flatMap(x => LazyList(x * 2)).toList shouldEqual ll.toList.map(_ * 2)

    }

    "append" in {
      LazyList().append(LazyList(0, 1, 2)).toList shouldEqual List(0, 1, 2)
      ll.append(LazyList(6, 7, 8)).toList shouldEqual List((0 to 8): _*)
    }

    "ones" in {
      ones.take(5).toList shouldEqual List.fill(5)(1)
      ones.map(_ + 1).exists(_ % 2 == 0) shouldBe true
      ones.forAll(_ != 1) shouldBe false
    }

    "fibs" in {
      LazyList.fibs.take(5).toList shouldEqual List(0, 1, 1, 2, 3)
    }

    "unfold and related implementations" in {
      LazyList.fibsViaUnfold.take(5).toList shouldEqual List(0, 1, 1, 2, 3)
      LazyList.fromViaUnfold(0).take(5).toList shouldEqual List(0, 1, 2, 3, 4)
      LazyList.continuallyViaUnfold(43).take(4).toList shouldEqual List.fill(4)(43)
      ll.takeViaUnfold(3).toList shouldEqual List(0, 1, 2)
      ll.takeWhileViaUnfold(_ < 3).toList shouldEqual List(0, 1, 2)

      LazyList(0, 1, 2).zipWith(LazyList.empty[Int])((x, y) => x * y).toList shouldEqual Nil
      LazyList(0, 1, 2).zipWith(LazyList(3, 4, 5))((x, y) => x * y).toList shouldEqual List(0, 4, 10)
      LazyList(0, 1, 2).zipWith(LazyList(3, 4, 5, 6))((x, y) => x * y).toList shouldEqual List(0, 4, 10)
      LazyList(0, 1, 2, 3).zipWith(LazyList(3, 4, 5))((x, y) => x * y).toList shouldEqual List(0, 4, 10)

      LazyList(0, 1, 2).zipAll(LazyList(3, 4, 5, 6)).toList shouldEqual
        LazyList(
          (Some(0), Some(3)),
          (Some(1), Some(4)),
          (Some(2), Some(5)),
          (None, Some(6))
        ).toList

      ll.hasSubsequence(LazyList(0, 1, 2)) shouldBe true
      ll.hasSubsequence(LazyList(0, 1, 2, 7)) shouldBe false

      LazyList(1, 2, 3).startsWith(LazyList(1, 2)) shouldBe true
      LazyList(1, 2, 3).startsWith(LazyList(0, 1)) shouldBe false
      LazyList(1, 2, 3).startsWith(LazyList()) shouldBe false

      LazyList(1, 2, 3).tails.toList.map(_.toList) shouldEqual
        List(List(1, 2, 3), List(2, 3), List(3))
    }
  }
}
