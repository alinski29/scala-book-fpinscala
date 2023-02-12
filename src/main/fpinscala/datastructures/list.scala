package fpinscala.datastructures

// +A = a is covariant, e.g. List[Dog] is a subtype of List[Animal], assuming Dog is a subtype of Animal
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  // apply is a variadic function - accepts varying number of arguments
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def tail[T](as: List[T]): List[T] = as match
    case Nil         => sys.error("tail of an empty list")
    case Cons(x, xs) => xs

  def setHead[T](as: List[T], h: T): List[T] = as match
    case Nil         => sys.error("head of an empty list")
    case Cons(x, xs) => Cons(h, xs)

  def drop[A](as: List[A], n: Int): List[A] = as match
    case Nil                  => as
    case Cons(_, _) if n == 0 => as
    case Cons(x, xs)          => drop(xs, n - 1)

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil         => as
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil         => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldRight(ns: List[Int]): Int = foldRight(ns, 0, (a, b) => a + b)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (b, a) => b + a)

  def productViaFoldRight(ns: List[Double]): Double = foldRight(ns, 1.0, (a, b) => a * b)
  
  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, (b, a) => b * a)

  // 3.9 Compute the length of a list using foldRight
  def lengthFoldRight[A](as: List[A]): Int = foldRight(as, 0, (_, n) => n + 1)
  
  def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0, (n, _) => n + 1)

// @main
def run(): Unit = {

  import List._

  val ex1: List[Double] = Nil
  val ex2: List[Int]    = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))

  println(ex2)
  println(ex3)

  // val myList = List.Cons(1, List.Cons(2, List.Cons(3, List.Cons(4, List.Nil))))
  val myList = List(1, 2, 3, 4, 5)
  println(List.sum(myList))

  List(1, 2, 3) match {
    case List.Cons(x, xs) => println(s"x: $x; xs: $xs")
    case List.Nil         => println("Nil")
  }

}
