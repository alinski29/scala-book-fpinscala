package fpinscala.monoids

import fpinscala.monoids.Monoid

// the underscore indicates that F is not a proper type but a type
// constructor that takes one type argument - higher-order type constructor / higher-kinded type
trait Foldable[F[_]]:
  import fpinscala.monoids.Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldMap(f.curried)(using endoMonoid[B])(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldMap(a => b => f(b, a))(using endoMonoid[B])(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldRight(mb.empty)((a, b) => mb.combine(f(a), b))

    def combineAll(using m: Monoid[A]): A =
      as.foldLeft(m.empty)(m.combine)

    /* Exercise 10.15
    Any Foldable structure can be turned into a List. Add a toList extension
    method to the Foldable trait and provide a concrete implementation in terms
    of the other methods on Foldable:
     */
    def toList: List[A] =
      as.foldRight(List.empty)(_ :: _)

/* Exercise 10.12
  Implement Foldable[List], Foldable[IndexedSeq], and Foldable[LazyList].
  Remember that foldRight, foldLeft, and foldMap can all be implemented in terms of each other,
  but that might not be the most efficient implementation. */

object Foldable:

  given Foldable[List] with {
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

      override def toList = as
  }

  given Foldable[IndexedSeq] with {
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)
  }

  given Foldable[LazyList] with {
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)
  }

  /* Exercise 10.13
  Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
   */
  import fpinscala.datastructures.Tree
  given Foldable[Tree] with {
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as match
          case Tree.Leaf(v) => f(v, acc)
          case Tree.Branch(l, r) =>
            l.foldRight(r.foldRight(acc)(f))(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as match
          case Tree.Leaf(v) => f(acc, v)
          case Tree.Branch(l, r) =>
            r.foldLeft(l.foldLeft(acc)(f))(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match
          case Tree.Leaf(v)      => f(v)
          case Tree.Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))
  }

  /* Exercise 10.14
  Write a Foldable[Option] instance.
   */

  given Foldable[Option] with {
    extension [A](opt: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        opt match
          case None    => acc
          case Some(a) => f(a, acc)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        opt match
          case None    => acc
          case Some(a) => f(acc, a)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        opt match
          case None    => mb.empty
          case Some(a) => f(a)
  }

end Foldable
