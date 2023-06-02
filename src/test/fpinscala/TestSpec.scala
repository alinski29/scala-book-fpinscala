//> using dep "org.scalatest::scalatest:3.2.15"
package fpinscala

import fpinscala.datastructures.Tree
import fpinscala.state.RNG
import fpinscala.state.State
import fpinscala.testing.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

trait TestSpec extends AnyWordSpecLike with should.Matchers:

  lazy val es                                             = Executors.newFixedThreadPool(4)
  lazy val genShortNumber: Gen[Int]                       = Gen.choose(0, 100)
  lazy val genChar: Gen[Char]                             = Gen.choose(97, 123).map(_.toChar)
  lazy val genInt: Gen[Int]                               = Gen(State(RNG.int))
  lazy val genDouble: Gen[Double]                         = Gen(State(RNG.double))
  lazy val genString: Gen[String]                         = genList(genChar).map(_.mkString)
  lazy val genRNG: Gen[RNG]                               = Gen(State(RNG.int)).map(i => RNG.SimpleRNG(i.toLong))
  lazy val genIntList: Gen[List[Int]]                     = genList(genInt)
  lazy val genDoubleList: Gen[List[Double]]               = genList(genDouble)
  lazy val genStringList: Gen[List[String]]               = genList(genString)
  lazy val genBooleanList: Gen[List[Boolean]]             = genList(Gen.boolean)
  lazy val genIntOption: Gen[Option[Int]]                 = genInt.map(i => if i % 2 == 0 then Some(i / 2) else None)
  lazy val genIntIndexedSeq: Gen[IndexedSeq[Int]]         = genList(genInt).map(_.toIndexedSeq)
  lazy val genDoubleIndexedSeq: Gen[IndexedSeq[Double]]   = genList(genDouble).map(_.toIndexedSeq)
  lazy val genStringIndexedSeq: Gen[IndexedSeq[String]]   = genList(genString).map(_.toIndexedSeq)
  lazy val genBooleanIndexedSeq: Gen[IndexedSeq[Boolean]] = genList(Gen.boolean).map(_.toIndexedSeq)
  lazy val genIntLazyList: Gen[LazyList[Int]]             = genLazyList(genInt)
  lazy val genDoubleLazyList: Gen[LazyList[Double]]       = genLazyList(genDouble)
  lazy val genStringLazyList: Gen[LazyList[String]]       = genLazyList(genString)
  lazy val genBooleanLazyList: Gen[LazyList[Boolean]]     = genLazyList(Gen.boolean)
  lazy val genIntTree: Gen[Tree[Int]]                     = genTree(genInt)

  def genList[A](g: Gen[A]): Gen[List[A]] =
    for
      n    <- genShortNumber
      list <- Gen.listOfN(n, g)
    yield list

  def genNonEmptyList[A](g: Gen[A]): Gen[List[A]] =
    for
      n    <- Gen.choose(1, 100)
      list <- Gen.listOfN(n, g)
    yield list

  def genLazyList[A](g: Gen[A]): Gen[LazyList[A]] =
    genList(g).map(xs => LazyList(xs*))

  private def genTree[A](g: Gen[A]): Gen[Tree[A]] =
    def loop(): Gen[Tree[A]] =
      Gen.boolean.flatMap {
        if _ then g.map(n => Tree.Leaf(n))
        else
          for
            left  <- loop()
            right <- loop()
          yield Tree.Branch(left, right)
      }
    loop()
