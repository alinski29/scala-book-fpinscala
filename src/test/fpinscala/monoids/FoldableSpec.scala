package fpinscala.monoids

import fpinscala.TestSpec
import fpinscala.monoids.Monoid
import fpinscala.monoids.Monoid.*
import fpinscala.testing.*
import fpinscala.parallelism.Nonblocking.*

class FoldableSpec extends TestSpec:
  import Foldable.given

  "Foldable trait" should {

    "fold Lists" in {
      Prop.forAll(genStringList) { xs =>
        val exp = xs.map(_.length).sum
        xs.foldRight(0)((s, acc) => s.length + acc) == exp
        xs.foldLeft(0)((acc, s) => s.length + acc) == exp
        xs.foldMap[Int](_.length) == exp
        xs.combineAll == xs.mkString
      }
    }

    "fold IndexedSeq" in {
      Prop.forAll(genStringIndexedSeq) { xs =>
        val exp = xs.map(_.length).sum
        xs.foldRight(0)((s, acc) => s.length + acc) == exp
        xs.foldLeft(0)((acc, s) => s.length + acc) == exp
        xs.foldMap[Int](_.length) == exp
        xs.combineAll == xs.mkString
      }
    }

    "fold Tree" in {
      Prop.forAll(genIntTree) { tree =>
        given Monoid[Int] = new:
          def combine(a1: Int, a2: Int): Int = a1 max a2
          val empty: Int                     = Int.MinValue

        val expected = tree.maximum
        tree.foldRight(Int.MinValue)(_ max _) == expected
        tree.foldLeft(Int.MinValue)(_ max _) == expected
        tree.foldMap[Int](identity) == expected
        tree.combineAll == expected
      }
    }

    "fold Option" in {
      Prop.forAll(genIntOption) { opt =>
        val exp = opt.getOrElse(0)
        opt.foldRight(0)(_ + _) == exp
        opt.foldLeft(0)(_ + _) == exp
        opt.foldMap[Int](identity) == exp
        opt.combineAll == exp
      }
    }

  }
