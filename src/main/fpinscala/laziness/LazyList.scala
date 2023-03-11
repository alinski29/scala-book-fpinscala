package fpinscala.laziness

import scala.annotation.tailrec
import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Some(h())

  /* Exercise 5.1
  Write a function to convert a LazyList to a List, which will force its evaluation and let you look
  at it in the REPL. You can convert to the regular List type in the standard library. You can place
  this and other functions that operate on a LazyList inside the LazyList enum.
   */
  def toListRecursive: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  /*
  The above solution will stack overflow for large lazy lists, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the lazy list. Then at the end we reverse the result to get the
  correct order again.
   */
  def toList: List[A] =
    @tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Empty      => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)
    go(this, Nil)

  /* Exercise 5.2
    Write the function take(n) for returning the first n elements of a LazyList, and drop(n) for
    skipping the first n elements of a LazyList. Define these functions inside the LazyList enum.
   */

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  /* Exercise 5.3
  Write the function takeWhile for returning all starting elements of a
  LazyList that match the given predicate.
   */
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  // supports early termination and subsequently is safe for use with infinite lazy lists
  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc

  def exists(p: A => Boolean) =
    this.foldRight(false)((a, b) => p(a) || b)

  /*  Exercise 5.4
  Implement forAll, which checks that all elements in the LazyList match a
  given predicate. Your implementation should terminate the traversal as soon
  as it encounters a nonmatching value.
   */

  /*
  Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
   */
  def forAll(p: A => Boolean) =
    this.foldRight(true)((a, b) => p(a) && b)

  /* Exercise 5.5: Use foldRight to implement takeWhile. */
  def takeWhileFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  /* Exercise 5.6: Implement headOption using foldRight. */
  def headOptionFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /* Exercise 5.7
  Implement map, filter, append, and flatMap using foldRight. The append method should
  be non-strict in its argument.
   */
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((h, acc) => if p(h) then cons(h, acc) else acc)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  /* Even though filter transforms the whole lazy list, that transformation
  is done lazily, so find terminates as soon as a match is found.
   */
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  /* Exercise 5.13
  Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
  and zipAll. The zipAll function should continue the traversal as long as
  either lazy list has more elements—it uses Option to indicate whether each
  lazy list has been exhausted.
   */

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(((f(h()), t())))
      case Empty      => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)            => Some((h(), (empty, 0)))
      case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), nn - 1)))
      case _                          => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      // case Cons(h, t) => Some( (, t()) )
      case _ => None
    }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty)               => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None) -> (t1(), Empty))
    }

  def hasSubsequence[B >: A](that: LazyList[B]): Boolean =
    that.forAll(x => this.exists(_ == x))
    // tails.exists(_.startsWith(that))

  /* Exercise 5.14
   Implement startsWith using functions you’ve written. It should check if one
   LazyList is a prefix of another. For instance:
   LazyList(1,2,3).startsWith(LazyList(1,2)) would be true.
   */
  def startsWith[A](prefix: LazyList[A]): Boolean =
    prefix match
      case Cons(_, _) => this.zipWith(prefix)((a, b) => a == b).forAll(x => x)
      case _          => false

  /* Exercise 5.15
  Implement tails using unfold. For a given LazyList, tails returns the LazyList of suffixes
  of the input sequence, starting with the original LazyList.
  ex: LazyList(1, 2, 3) = LazyList(LazyList(1, 2, 3), LazyList(2, 3), LazyList(3), LazyList())
   */
  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _          => None
    }.append(LazyList.empty)

  /* Exercise 5.16
  Hard: Generalize tails to the function scanRight, which is like a foldRight that returns
  a lazy list of the intermediate results. For example:
  LazyList(1, 2, 3).scanRight(0)(_ + _).toList =  List(6, 5, 3, 0)
   */
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)) { (a, b0) =>
      // b0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val b1 = b0
      val b2      = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    }(1)

object LazyList:
  // smart constructor - convention to lowercase the first letter of the coresponding data constructor
  // memoizes the by-name arguments for the head and tail - ensures our thunk will do its work only once, when forced for the 1st time.
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*)) // scala wraps the arguments of cons in thunks

  val ones: LazyList[Int] =
    LazyList.cons(1, ones)

  /* Exercise 5.8
  Generalize ones slightly to the function continually, which returns an infinite LazyList of a given value.
   */
  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, continually(a))

  /* Exercise 5.9
  Write a function that generates an infinite lazy list of integers, starting from n, then n + 1, n + 2, and so on.
   */
  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  /* Exercise 5.10
  Write a function fibs that generates the infinite lazy list of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  val fibs: LazyList[Int] =
    def go(curr: Int, next: Int): LazyList[Int] =
      cons(curr, go(next, curr + next))
    go(0, 1)

    /* go(0, 1) =>
    cons(0, go(1, 0 + 1)) =>
    cons(1, go(1, 1 + 1)) =>
    cons(1, go(1, 1 + 1)) =>
    cons(1, go(2, 2 + 1)) =>
    cons(2, go(3, 3 + 2))
    0, 1, 1, 2, 3
     */

  /* Exercise 5.11
   Write a more general LazyList-building function called unfold. It takes an
   initial state, and a function for producing both the next state and the next
   value in the generated lazy list.
   */
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((nv, ns)) => cons(nv, unfold(ns)(f))
      case None           => empty
  // corecursive function: produce data and continue to run as long as they are productive

  /* Exercise 5.12
  Write fibs, from, continually, and ones in terms of unfold
   */
  val fibsViaUnfold: LazyList[Int] =
    // unfold((0, 1)){ (curr: Int, next: Int) => Some(curr, (next, curr + next))  }
    unfold((0, 1)) { case (curr, next) => Some(curr, (next, curr + next)) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n) { case n => Some(n, n + 1) }

  def continuallyViaUnfold[A](a: A) =
    unfold(a) { case a => Some(a, a) }

// def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
//   if cond then onTrue() else onFalse()

// def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
//   if cond then onTrue else onFalse

// def maybeTwice(b: Boolean, i: => Int) = if b then i + i else i

// def maybeTwice2(b: Boolean, i: => Int) =
//   // cache the valyue explicitly
//   lazy val j = i
//   if b then j +j else 0

// @main
// def run(): Unit =
//   val x = maybeTwice(true, { println("hi"); 1 + 41 })
//   val x2 = maybeTwice2(true, { println("hi"); 1 + 41 })
//   // if2(a < 22, () => println("a"), () => println("b"))
