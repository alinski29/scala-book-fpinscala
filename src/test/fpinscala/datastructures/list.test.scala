package fpinscala.datastructures

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import List._

class ListSpec extends AnyFlatSpec with should.Matchers {
  "List implementation" should "work as expected" in {
    val myList = List(1, 2, 3, 4, 5)

    val result = myList match
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this should be matched
      case Cons(h, t)                            => h + sum(t)
      case Nil                                   => 42
    
    result shouldEqual 3 

    sum(List.Nil) shouldEqual 0
    sum(myList) shouldEqual 15

    product(List.Nil) shouldEqual 1
    product(List(0, 1, 2, 3)) shouldEqual 0

    assertThrows[RuntimeException](tail(List.Nil))
    tail(myList) shouldEqual List(2, 3, 4, 5)

    assertThrows[RuntimeException](setHead(List.Nil, 0))
    setHead(myList, 0) shouldEqual List(0, 2, 3, 4, 5)

    drop(List.Nil, 1) shouldEqual List.Nil
    drop(myList, 6) shouldEqual List.Nil
    drop(myList, 2) shouldEqual List(3, 4, 5) 

    dropWhile(myList, (x: Int) => x % 2 == 0) shouldEqual List(1, 3, 5)
    dropWhile(myList, (x: Int) => x > 10) shouldEqual myList
    dropWhile(myList, (x: Int) => x < 10) shouldEqual List.Nil

    append(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    append(List.Nil, List(3, 4)) shouldEqual List(3, 4)
    append(List(1, 2), List.Nil) shouldEqual List(1, 2)

    sumViaFoldRight(myList) shouldEqual 15
    sumViaFoldRight(List.Nil) shouldEqual 0

    productViaFoldRight(List((1 to 5).map(_.toDouble): _*)) shouldEqual 120
    productViaFoldRight(List.Nil) shouldEqual 1
    
    lengthFoldRight(myList) shouldEqual 5
    lengthFoldRight(List.Nil) shouldEqual 0
    
    sumViaFoldLeft(myList) shouldEqual 15
    sumViaFoldLeft(List.Nil) shouldEqual 0

    productViaFoldLeft(List((1 to 5).map(_.toDouble): _*)) shouldEqual 120
    productViaFoldLeft(List.Nil) shouldEqual 1

    lengthFoldLeft(myList) shouldEqual 5
    lengthFoldLeft(List.Nil) shouldEqual 0
  }
}
