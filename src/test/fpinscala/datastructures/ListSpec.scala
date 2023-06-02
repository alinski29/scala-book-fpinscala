package fpinscala.datastructures

import fpinscala.TestSpec
import fpinscala.datastructures.List
import fpinscala.datastructures.List.{Nil, Cons}

class ListSpec extends TestSpec {

  "List implementation" should {

    val l = List((1 to 5)*)

    "implement sum and product" in {
      List.sum(l) shouldEqual 15
      List.product(List((1 to 5).map(_.toDouble)*)) shouldEqual 120
    }

    "implement tail" in {
      assertThrows[RuntimeException](List.tail(List()))
      List.tail(l) shouldEqual List(2, 3, 4, 5)
    }

    "set head" in {
      assertThrows[RuntimeException](List.setHead(Nil, 0))
      List.setHead(l, 0) shouldEqual List(0, 2, 3, 4, 5)
    }

    "drop elements" in {
      List.drop(List(), 1) shouldEqual List()
      List.drop(l, 0) shouldEqual l
      List.drop(l, 2) shouldEqual List(3, 4, 5)

      List.dropWhile(l, x => x % 2 != 0) shouldEqual List(2, 4)
      List.dropWhile(List[Int](), x => x % 2 != 0) shouldEqual Nil
    }

    "append" in {
      List.append(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
      List.append(Nil, List(3, 4)) shouldEqual List(3, 4)
      List.append(List(1, 2), Nil) shouldEqual List(1, 2)
      List.append(Nil, Nil) shouldEqual Nil
    }

    "init" in {
      assertThrows[RuntimeException](List.init(Nil))
      List.init(l) shouldEqual List(1, 2, 3, 4)
      List.init(List(1)) shouldEqual Nil
    }

    "compute sum, product and length via foldRight" in {
      List.sumViaFoldRight(Nil) shouldEqual 0
      List.sumViaFoldRight(l) shouldEqual List.sum(l)

      val ld = List((0 to 5).map(_.toDouble)*)
      List.productViaFoldRight(ld) shouldEqual List.product(ld)
      List.productViaFoldRight(Nil) shouldEqual 1

      List.lengthViaFoldRight(Nil) shouldEqual 0
      List.lengthViaFoldRight(l) shouldEqual 5
    }

    "compute sum, product and length via foldLeft" in {
      List.sumViaFoldLeft(Nil) shouldEqual 0
      List.sumViaFoldLeft(l) shouldEqual List.sum(l)

      List.productViaFoldLeft(Nil) shouldEqual 1
      val ld = List((0 to 5).map(_.toDouble)*)
      List.productViaFoldLeft(ld) shouldEqual List.product(ld)

      List.lengthViaFoldLeft(Nil) shouldEqual 0
      List.lengthViaFoldLeft(l) shouldEqual 5
    }

    "reverse using foldLeft" in {
      List.reverse(Nil) shouldEqual Nil
      List.reverse(l) shouldEqual List(5, 4, 3, 2, 1)
    }

    "append via foldRight and foldLeft" in {
      List.appendViaFoldRight(List(1, 2, 3), List(4, 5)) shouldEqual List(1, 2, 3, 4, 5)
      List.appendViaFoldRight(Nil, List(3, 4)) shouldEqual List(3, 4)
      List.appendViaFoldRight(List(1, 2), Nil) shouldEqual List(1, 2)
      List.appendViaFoldRight(Nil, Nil) shouldEqual Nil

      List.appendViaFoldLeft(List(1, 2, 3), List(4, 5)) shouldEqual List(1, 2, 3, 4, 5)
      List.appendViaFoldLeft(Nil, List(3, 4)) shouldEqual List(3, 4)
      List.appendViaFoldLeft(List(1, 2), Nil) shouldEqual List(1, 2)
      List.appendViaFoldLeft(Nil, Nil) shouldEqual Nil
    }

    "flatten list of lists into one list" in {
      List.flatten(List(List(1, 2), List(3, 4))) shouldEqual List(1, 2, 3, 4)
      List.flatten(List(List[Int](), List(3, 4))) shouldEqual List(3, 4)
      List.flatten(List(List(1, 2), List[Int]())) shouldEqual List(1, 2)
      List.flatten(List(Nil)) shouldEqual Nil
    }

    "add 1 to each element of the list" in {
      List.incrementEach(l) shouldEqual List(2, 3, 4, 5, 6)
    }

    "convert list[dobule] -> list[string]" in {
      List.doubleToString(List(1.0, 1.01, 2.0)) shouldEqual List("1.0", "1.01", "2.0")
    }

    "implement map" in {
      List.map(l, x => x + 1) shouldEqual List(2, 3, 4, 5, 6)
      List.map(l, x => x * 2) shouldEqual List(2, 4, 6, 8, 10)
      List.map(Nil: List[Int], x => x + 1) shouldEqual Nil
    }

    "implement filter" in {
      List.filter(l, i => i % 2 == 0) shouldEqual List(2, 4)
      List.filter(List[Int](), i => i % 2 == 0) shouldEqual Nil
      List.filterViaFlatMap(l, i => i % 2 == 0) shouldEqual List(2, 4)
    }

    "implement flatMap" in {
      List.flatMap(Nil, i => List(i, i)) shouldEqual Nil
      List.flatMap(List(1, 2, 3), i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
      List.flatMap_1(List(1, 2, 3), i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
    }

    "implement zip related functions" in {
      List.zipAdd(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
      List.zipAdd(List(1, 2), List(4, 5, 6)) shouldEqual List(5, 7)
      List.zipAdd(List(1, 2, 3), List(4, 5)) shouldEqual List(5, 7)

      List.zipWith(List(1, 2), List(3, 4), (a, b) => a * b) shouldEqual List(3, 8)
      List.zipWith(List(1, 2, 3), List(3, 4), (a, b) => a * b) shouldEqual List(3, 8)
      List.zipWith(List(1, 2), List(3, 4, 5), (a, b) => a * b) shouldEqual List(3, 8)
      List.zipWith(Nil: List[Int], List(3, 4, 5), (a, b) => a * b) shouldEqual Nil

      List.zipWith(List("foo", "badum"), List("Bar", "Tzz"), (a, b) => a + b) shouldEqual List("fooBar", "badumTzz")
    }

    "implement hasSubsequence" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
      List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)) shouldBe true
      List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)) shouldBe false
    }
  }
}
