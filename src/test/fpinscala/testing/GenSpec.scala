package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.state.RNG.*
import fpinscala.state.*

import fpinscala.testing.Gen.*
import fpinscala.testing.*

import fpinscala.TestSpec

class GenSpec extends TestSpec {

  "Gen" should {
    "forall" in {

      val rng      = RNG.SimpleRNG(42)
      val smallInt = Gen.choose(-10, 10)

      val maxProp = Prop.forAll(smallInt.nonEmptyList) { l =>
        val max = l.max
        l.forall(_ <= max)
      }

      val sortedProp = Prop.forAll(Gen.choose(0, 10).nonEmptyList) { l =>
        val ls = l.sorted
        ls.zip(ls.tail).forall(_ <= _)
      }
      println(sortedProp.run())

    }
  }

}
