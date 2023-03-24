package fpinscala.monoids

import fpinscala.TestSpec
import fpinscala.monoid.Monoid
import fpinscala.monoid.Monoid.*
import fpinscala.monoid.Monoid.given
import fpinscala.parallelism.Nonblocking.*
import fpinscala.testing.*

class MonoidSpec extends TestSpec:

  "A monoid" should {

    import Prop.Result

    "test the monoid laws" in {
      monoidLaws(intAddition, Gen.choose(Int.MinValue, Int.MaxValue)).check() shouldEqual Result.Passed
      monoidLaws(
        intMultiplication,
        Gen.choose(
          math.sqrt(Int.MinValue.toDouble).toInt,
          math.sqrt(Int.MaxValue.toDouble).toInt
        )
      ).check() shouldEqual Result.Passed

      monoidLaws(stringMonoid, Gen.stringN(15)).check() shouldEqual Result.Passed
      monoidLaws(booleanOr, Gen.boolean).check() shouldEqual Result.Passed
      monoidLaws(booleanAnd, Gen.boolean).check() shouldEqual Result.Passed
      monoidLaws(
        optionMonoid,
        Gen.choose(0, 10).map(x => if x % 2 == 0 then Some(x) else None)
      ).check() shouldEqual Result.Passed

      monoidLaws(wcMonoid, wcGen).check() shouldEqual Result.Passed
    }

    "combine all elements" in {
      Prop.forAll(genStringList) { xs => combineAll(xs, stringMonoid) == xs.mkString }
      Prop.forAll(genIntList) { xs => combineAll(xs, intAddition) == xs.sum }
      Prop.forAll(genNonEmptyList(genInt)) { xs => combineAll(xs, intMultiplication) == xs.product }
      Prop.forAll(genBooleanList) { xs => combineAll(xs, booleanOr) == xs.exists(identity) }
      Prop.forAll(genBooleanList) { xs => combineAll(xs, booleanOr) == xs.forall(identity) }
    }

    "promote a Monoid[A] to a Par[Monoid[A]]" in {
      Prop.forAll(genString ** genString ** genString) { case ((s1, s2), s3) =>
        val (a, b, c) = (unit(s1), unit(s2), unit(s3))
        val m         = par(stringMonoid)

        m.combine(a, m.combine(b, c)).run(es) == m.combine(m.combine(a, b), c).run(es)
        m.combine(a, m.empty).run(es) == a.run(es)
        m.combine(m.empty, a).run(es) == a.run(es)
      }
    }

    "be able to do word count" in {
      wordCount("monoids are a cool concept") shouldEqual 5
      wordCount("mon ") shouldEqual 1
      wordCount("") shouldEqual 0
      wordCount(" ") shouldEqual 0
    }

    "do mapMerge" in {
      import Monoid.given
      val M  = mapMergeMonoid[String, Map[String, Int]]
      val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
      val m2 = Map("o1" -> Map("i2" -> 3))
      val m3 = M.combine(m1, m2)
      m3 shouldEqual Map("o1" -> Map("i1" -> 1, "i2" -> 5))
    }

    "function monoid" in {
      Prop.forAll(genInt) { a =>
        val m: Monoid[Int => String] = functionMonoid[Int, String]
        val f: Int => String         = i => if i % 2 == 0 then "even" else "odd"
        val g: Int => String         = i => if i < 0 then "negative" else "positive"
        val h: Int => String         = i => i.toString

        m.combine(f, m.empty)(a) == f(a)
        m.combine(m.empty, f)(a) == f(a)
        m.combine(f, m.combine(g, h))(a) == m.combine(m.combine(f, g), h)(a)
      }
    }

    "compute bag of words using a monoid" in {
      bag(IndexedSeq.empty[String]) shouldEqual Map.empty[String, Int]
      bag(IndexedSeq("foo")) shouldEqual Map("foo" -> 1)
      bag(IndexedSeq("foo", "foo", "foo")) shouldEqual Map("foo" -> 3)
      bag(IndexedSeq("foo", "foo", "bar", "baz")) shouldEqual Map("foo" -> 2, "bar" -> 1, "baz" -> 1)
    }

    // "do foldMap in parallel" in {
    //   import java.util.concurrent.Executors
    //   val es = Executors.newCachedThreadPool()

    //   Prop.forAll(genIntIndexedSeq) { xs =>
    //     val res = parFoldMap(xs, intAddition)(x => x).run(es)
    //     println(res)
    //     res == xs.sum
    //   }.run()
    // }
  }

end MonoidSpec
