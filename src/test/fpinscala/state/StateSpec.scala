package fpinscala.state

import fpinscala.state.CandyDispenser
import fpinscala.state.Input
import fpinscala.state.Machine
import fpinscala.state.RNG
import fpinscala.state.RNG.*
import org.scalatest.matchers.*
import org.scalatest.propspec.AnyPropSpecLike

class StateSpec extends AnyPropSpecLike with should.Matchers:
  import StateSpec.*

  private val rng = RNG.SimpleRNG(42)

  property("SimpleRNG should be referentially transparent for any seed") {
    val seeds = List(42, 0, -100, Int.MaxValue, Int.MinValue)
    seeds.forall(s => nextIntNTimes(s, 10) == nextIntNTimes(s, 10)) shouldBe true
  }

  property("Randon generation of non negative integers") {
    checkNTimes(rng, 100)(nonNegativeInt, (i, _) => i > 0) shouldBe true
  }

  property("Random generation of doubles between 0 and 1") {
    checkNTimes(rng, 100)(double, (i, _) => i >= 0 && i < 1) shouldBe true
  }

  property("Random generation of non negative evens") {
    checkNTimes(rng, 100)(nonNegativeEven, (i, _) => i % 2 == 0) shouldBe true
  }

  property("map2 implementation should combine 2 actions") {
    val doubleEvensPair = map2(double, nonNegativeEven)((a, b) => (a, b))
    val ((d, e), _)     = doubleEvensPair(rng)
    checkNTimes(rng, 10)(
      doubleEvensPair,
      (x, _) => x._1 >= 0 && x._1 > 1 && x._2 % 2 == 0
    )
  }

  property("sequence implementation should combine multiple actions into a single one") {
    val actions = List(int, nonNegativeEven)
    checkNTimes(rng, 10)(
      sequence(actions),
      (rns, _) =>
        rns match
          case i :: e :: Nil => e % 2 == 0
          case _             => false
    )
  }

  property("flatMap implementation and functions using flatMap") {
    checkNTimes(rng, 100)(flatMap(nonNegativeEven)(i => unit(i)), (i, _) => i % 2 == 0) shouldBe true
    checkNTimes(rng, 100)(nonNegativeLessThan(100), (i, _) => i < 100) shouldBe true

    checkNTimes(rng, 10)(
      mapViaFlatMap(nonNegativeEven)(a => a),
      (x, _) => x > 0
    )

    checkNTimes(rng, 10)(
      map2ViaFlatMap(double, nonNegativeEven)((a, b) => (a, b)),
      (x, _) => x._1 >= 0 && x._1 > 1 && x._2 % 2 == 0
    )
  }

  property("functional die roll") {
    checkNTimes(rng, 100)(rollDie, (i, _) => i > 0 && i <= 6)
  }

  property("candy dispenser") {
    import Input.{Coin, Turn}
    import CandyDispenser.*

    simulateMachine(List(Coin, Turn, Coin, Turn))
      .run(Machine(true, 2, 0))
      ._2 shouldEqual Machine(true, 0, 2)

    simulateMachine(List(Coin, Coin))
      .run(Machine(true, 2, 0))
      ._2 shouldEqual Machine(false, 2, 1)

    simulateMachine(List(Turn, Coin, Turn))
      .run(Machine(true, 2, 0))
      ._2 shouldEqual Machine(true, 1, 1)

  }

object StateSpec:

  def nextIntNTimes(seed: Long, n: Int): Seq[(Int, RNG)] =
    val gen = RNG.SimpleRNG(seed)
    (0 to n).map(_ => gen.nextInt)

  def checkNTimes[A](rng: RNG, n: Int)(f: Rand[A], prop: (A, RNG) => Boolean): Boolean =
    @annotation.tailrec
    def go(rng: RNG, n: Int, propState: Boolean): Boolean =
      if !propState then false
      else
        val (i, r) = f(rng)
        // println(s"($i, $r)")
        val propCheck = prop(i, r)
        if n == 0 then propState && propCheck
        else go(r, n - 1, propCheck)
    go(rng, n, true)
