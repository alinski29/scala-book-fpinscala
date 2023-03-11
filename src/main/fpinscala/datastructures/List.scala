package fpinscala.datastructures

import scala.annotation.tailrec

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

  /* Exercise 3.2
  Implement the function tail for removing the first element of a List
   */
  def tail[T](as: List[T]): List[T] = as match
    case Nil         => sys.error("tail of an empty list")
    case Cons(x, xs) => xs

  /* Exercise 3.3
  Implement the function setHead for replacing the first element of a List with a different value
   */
  def setHead[T](as: List[T], h: T): List[T] = as match
    case Nil         => sys.error("head of an empty list")
    case Cons(x, xs) => Cons(h, xs)

  /* Exercise 3.4
  Implement the function drop, which removes the first n elements from a list.
  Dropping n elements from an empty list should return the empty list
   */
  def drop[A](as: List[A], n: Int): List[A] = as match
    case Nil                  => as
    case Cons(_, _) if n == 0 => as
    case Cons(x, xs)          => drop(xs, n - 1)

  /* Exercise 3.5
  Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil         => as
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))

  /* Note that this definition only copies values until the first list is exhausted, so
    its runtime and memory usage are determined only by the length of a1. The
    remaining list then just points to a2 */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))

  /* Exercise 3.6
  Implement a function, init, that returns a List consisting of all but the last element of a List.
  So, given List(1,2,3,4), init will return List(1,2,3)
   */
  def init[A](as: List[A]): List[A] = as match
    case Nil          => sys.error("init of an empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil         => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int = foldRight(ns, 0, (a, b) => a + b)

  def productViaFoldRight(ns: List[Double]): Double = foldRight(ns, 1.0, (a, b) => a * b)

  /* Exercise 3.7
  Can product, implemented using foldRight, immediately halt the recursion
  and return 0.0 if it encounters a 0.0? Why or why not? Consider how any
  short-circuiting might work if you call foldRight with a large list

  No, this is not possible. foldRight recurses all the way to the end of the list
  before invoking the function
   */

  /* Exercise 3.8
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _)).

  Answer: Cons(1,Cons(2,Cons(3,Nil)))
  Recall that foldRight(as, acc, f) replaces Nil with acc and Cons with f.
  When we set acc to Nil and f to Cons, our replacements are all identities.
   */

  /* Exercise 3.9
  Compute the length of a list using foldRight
   */
  def lengthViaFoldRight[A](as: List[A]): Int = foldRight(as, 0, (_, n) => n + 1)

  /* Exercise 3.10
  Iimplementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe)
  Write another general list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed
  in the previous chapter. Start collapsing from the leftmost start of the list.
   */
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil         => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  /* Exercise 3.11
  Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (b, a) => b + a)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, (b, a) => b * a)

  def lengthViaFoldLeft[A](as: List[A]): Int = foldLeft(as, 0, (n, _) => n + 1)

  /* Exercise 3.12
  Write a function that returns the reverse of a list
  List(1,2,3) returns List(3,2,1). See if you can write it using a fold.
   */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A](), (acc, a) => Cons(a, acc))

  /* Exercise 3.13
  Hard: Can you write foldRight in terms of foldLeft? How about the other
  way around? Implementing foldRight via foldLeft is useful because it lets
  us implement foldRight tail-recursively, which means it works even for
  large lists without overflowing the stack.
   */

  // with 2 pases, one for reversing the list and one for the fold
  // def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  //   foldLeft(reverse(as), acc, (b, acc) => f(acc, b))

  // very complex, see book for references
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    foldRight(as, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  /* Exercise 3.14
  Implement append in terms of either foldLeft or foldRight instead of structural recursion.
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, (a, acc) => Cons(a, acc))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2, (acc, a) => Cons(a, acc))

  /* Exercise 3.15
  Hard: Write a function that concatenates a list of lists into a single list. Its
  runtime should be linear in the total length of all lists. Try to use functions we
  have already defined.
   */
  def flatten[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, Nil: List[A], (acc, a) => append(acc, a))
    // foldRight(l, Nil: List[A], append) // also works

  /* Exercise 3.16
  Write a function that transforms a list of integers by adding 1 to each element
  (that is, given a list of integers, returns a new list of integers where each value
  is one more than corresponding value in the original list).
   */
  def incrementEach(as: List[Int]): List[Int] =
    foldRightViaFoldLeft(as, Nil: List[Int], (a, acc) => Cons(a + 1, acc))

  /* Exercise 3.17
  Write a function that turns each value in a List[Double] into a String
   */
  def doubleToString(as: List[Double]): List[String] =
    foldRightViaFoldLeft(as, Nil: List[String], (a, acc) => Cons(a.toString, acc))

  /* Exercise 3.18
  Write a function map that generalizes modifying each element in a list
  while maintaining the structure of the list.
   */
  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

  /* Exercise 3.19
  Write a function filter that removes elements from a list unless they satisfy a given predicate
   */
  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  /* Exercise 3.20
  Write a function flatMap that works like map except that the function given
  will return a list instead of a single result, and that list should be inserted into
  the final resulting list.
   */
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B], (a, acc) => append(f(a), acc))

  def flatMap_1[A, B](as: List[A], f: A => List[B]): List[B] =
    flatten(map(as, f))

  /* Exercise 3.21
  Use flatMap to implement filter
   */
  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  /* Exercise 3.22
  Write a function that accepts two lists and constructs a new list by adding
  corresponding elements.
  List(1,2,3) and List(4,5,6) => List(5,7,9).
   */
  def zipAdd(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
      case _                            => Nil

  /* Exercise 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition
   */
  def zipWith_1[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
      case _                            => Nil

  /* To make this stack safe, we can pass the accumulated value in to our
  recursive call instead of first recursing and then using the result in subsequent
  computation: */
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    @tailrec
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
      case _                            => acc
    reverse(loop(a, b, Nil))

  /* Exercise 3.24
  Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence
  List(1,2,3,4) would have List(1,2), List(2,3), and List(4), among others
   */
  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match
      case (_, Nil)                              => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _                                     => false

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h, t)                => hasSubsequence(t, sub)
