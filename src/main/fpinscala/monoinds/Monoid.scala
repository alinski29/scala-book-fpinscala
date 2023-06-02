package fpinscala.monoids

import fpinscala.testing.{Prop, Gen}
import fpinscala.parallelism.Nonblocking.*

/* A monoid consists of the following:
 - Some type A
 - An associative binary operation, combine, that takes two values of type A
 and combines them into one: combine(combine(x, y), z) == combine(x, combine(y, z))
 for any choice of x: A, y: A, z: A
 - A value, empty: A, that is an identity for that operation:
 combine(x, empty) == x and combine(empty, x) == x for any x: A
 */

/* Stated tersely, a monoid is a type together
  with a binary operation (combine) over that type, satisfying associativity and
  having an identity element (empty).
 */

trait Monoid[A]:
  def empty: A
  def combine(a1: A, a2: A): A

object Monoid:

  given stringMonoid: Monoid[String] = new {
    def combine(a1: String, a2: String) = a1 + a2
    def empty                           = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new {
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty                             = Nil
  }

  /* Exercise 10.1
      Give Monoid instances for integer addition and multiplication as well as the
      Boolean operators.
   */
  given intAddition: Monoid[Int] with {
    def combine(a1: Int, a2: Int) = a1 + a2
    def empty                     = 0
  }

  val intMultiplication: Monoid[Int] = new {
    def combine(a1: Int, a2: Int) = a1 * a2
    def empty                     = 1
  }

  val booleanOr: Monoid[Boolean] = new {
    def combine(a1: Boolean, a2: Boolean) = a1 || a2
    val empty                             = false
  }

  val booleanAnd: Monoid[Boolean] = new {
    def combine(a1: Boolean, a2: Boolean) = a1 & a2
    val empty                             = true
  }

  /* Exercise 10.2
  Give a Monoid instance for combining Option values. def optionMonoid[A]: Monoid[Option[A]]
   */

  def optionMonoid[A]: Monoid[Option[A]] = new {
    def combine(a1: Option[A], a2: Option[A]) =
      a1 orElse a2
    val empty = None
  }

  // We can get the dual of any monoid just by flipping the `combine`.
  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty                  = m.empty

  /* Exerice 10.3
   A function having the same argument and return type is sometimes called an endofunction.
   Write a monoid for endofunctions.
   */
  def endoMonoid[A]: Monoid[A => A] = new {
    val empty                           = identity
    def combine(a1: A => A, a2: A => A) = a1 andThen a2
  }

  /* Exercise 10.4
    Use the property-based testing framework we developed in part 2 to implement a property for
    the monoid laws. Use your property to test the monoids we’ve written.
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val identity = Prop
      .forAll(gen) { a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      }
      .tag("identity")
    val asociativity = Prop
      .forAll(gen ** gen ** gen) { case ((x, y), z) =>
        m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))
      }
      .tag("asociativity")

    identity && asociativity

  val words = List("Hic", "Est", "Index")
  val s     = words.foldRight(stringMonoid.empty)(stringMonoid.combine)
  val t     = words.foldLeft(stringMonoid.empty)(stringMonoid.combine)
  // words.foldLeft("")(_ + _) == (("" + "Hic") + "Est") + "Index"
  // words.foldRight(""(_ + _) = "Hic" + ("Est" + ("Index" + ""))

  // a more general function that folds a list with a monoid
  def combineAll[A](as: List[A], m: Monoid[A]) =
    as.foldLeft(m.empty)(m.combine)

  /* Exerice 10.5
   Implement foldMap.
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) =
    as.foldLeft(m.empty)((acc, a) => m.combine(acc, f(a)))

  /* Exericse 10.6
  Hard: The foldMap function can be implemented using either foldLeft or
  foldRight. But you can also write foldLeft and foldRight using foldMap! Try it.
   */
  def foldLeftViaFoldMap[A, B](as: List[A])(init: B)(f: (B, A) => B) =
    foldMap(as, endoMonoid)(a => b => f(b, a))(init)

  /* Exericse 10.7
   Implement a foldMap for a IndexedSeq. Your implementation should use the strategy of
   splitting the sequence in two, recursively processing each half, and then adding
   the answers together with the monoid.
   */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then m.empty
    else if as.length == 1 then f(as.head)
    else
      val (lhs, rhs) = as.splitAt(as.size / 2)
      m.combine(foldMapV(lhs, m)(f), foldMapV(rhs, m)(f))

  /* Exercise 10.8 (optional)
  Hard: Also implement a parallel version of foldMap using the library we
  developed in chapter 7. Hint: Implement par, a combinator to promote
  Monoid[A] to a Monoid[Par[A]],[118] and then use this to implement
  parFoldMap.
   */
  import fpinscala.parallelism.Nonblocking.Par.*

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new {
      val empty = unit(m.empty)
      def combine(a: Par[A], b: Par[A]): Par[A] =
        a.map2(b)(m.combine)
    }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    // parMap(as)(f).flatMap { bs =>
    //   foldMapV(bs, par(m))(b => lazyUnit(b))
    // }
    for
      bs  <- parMap(as)(f)
      res <- foldMapV(bs, par(m))(b => lazyUnit(b))
    yield res

  /* Exercise 10.9
  Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
  You’ll need to come up with a creative Monoid.
   */
  // val sortedMonoid: Monoid[Boolean] = new {
  //   val empty = true
  //   def combine(a: Boolean, b: Boolean) = a <= b
  // }

  // foldMap(List(1, 3, 2), sortedMonoid)(x => x > )

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  /* Exercise 10.10
  Write a monoid instance for WC and make sure that it meets the monoid laws.
  val wcMonoid: Monoid[WC]
   */

  // wc("lorem ipsum do") = Part("lorem", 1, "do") only 1 word we know for sure ("lorem")
  // wc("lor sit amet, ") = Part("lor", 2, "")

  import WC.*
  val wcMonoid: Monoid[WC] = new {
    val empty = Stub("")

    def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b))       => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l, w, r), WC.Part(l2, w2, r2)) =>
        WC.Part(l, w + (if (r + l2).isEmpty then 0 else 1) + w2, r2)
  }

  def wcGen: Gen[WC] =
    val smallString = Gen.choose(0, 10).flatMap(Gen.stringN)
    val genStub     = smallString.map(s => WC.Stub(s))
    val genPart = for
      lStub <- smallString
      words <- Gen.choose(0, 10)
      rStub <- smallString
    yield WC.Part(lStub, words, rStub)
    Gen.union(genStub, genPart)

  /* Exercise 10.11
  Use the WC monoid to implement a function that counts words in a String by recursively
  splitting it into substrings and counting the words in those substrings.
   */
  def wordCount(s: String): Int =
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace then WC.Part("", 0, "")
      else WC.Stub(c.toString)
    def unstub(s: String) = if s.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(s)       => unstub(s)
      case WC.Part(l, n, r) => unstub(l) + n + unstub(r)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(using m: Monoid[B]): B =
    foldMapV(as, m)(f)

  val strings = List("abra", "ca", "dabra")
  foldMap(strings.toIndexedSeq)(_.length)

  /* Exercise 10.16
  Implement productMonoid using a ma: Monoid[A] and mb: Monoid[B].
  Notice that your implementation of combine is associative so long as
  ma.combine and mb.combine are both associative.
   */
  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new {
      val empty = (ma.empty, mb.empty)
      def combine(a1: (A, B), a2: (A, B)): (A, B) =
        (ma.combine(a1._1, a2._1), mb.combine(a1._2, a2._2))
    }

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with {
    val empty = Map()
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty) { (acc, k) =>
        acc.updated(k, mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty)))
      }
  }

  val m = summon[Monoid[Map[String, Map[String, Int]]]]
  // m: MapMergeMonoid[String, Map[String, Int]]
  m.combine(
    Map("o1" -> Map("i1" -> 1, "i2" -> 2)),
    Map("o1" -> Map("i2" -> 3))
  )

  /* Exercise 10.17
    Write a monoid instance for functions whose results are monoids.
   */
  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with {
    val empty: A => B = (a: A) => mb.empty

    def combine(f: A => B, g: A => B): A => B =
      (a: A) => mb.combine(f(a), g(a))
  }

  import fpinscala.monoids.Foldable.given

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    val bagMonoid = mapMergeMonoid[A, Int](using intAddition)
    foldMapV(as, bagMonoid)(a => Map(a -> 1))

  def bag_1[A](as: IndexedSeq[A]): Map[A, Int] =
    as.foldMap(a => Map(a -> 1))

  // due to monoid composition, we can perform multiple calculations simultanely
  val (lSize, lSum) = List(1, 2, 3, 4).foldMap(a => (1, a))
  // scala derives intAddition monoid and productMonoid
  val mean = lSize / lSum

end Monoid
