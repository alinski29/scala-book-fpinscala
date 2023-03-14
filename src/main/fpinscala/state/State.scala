package fpinscala.state

trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)

  /* Exercise 6.1
    Write a function that uses RNG.nextInt to generate a random integer between
    0 and Int.MaxValue (inclusive). Make sure to handle the corner case when
    nextInt returns Int.MinValue, which doesn’t have a non-negative
    counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, s) = rng.nextInt
    ((if i < 0 then (-i + 1) else i), s)

  /* Exercise 6.2
    Write a function to generate a Double between 0 and 1, not including 1. Note:
    You can use Int.MaxValue to obtain the maximum positive integer value,
    and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) =
    val (i, s) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1.0), s)

  /* Exercise 6.3
    Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
    and a (Double, Double, Double) 3-tuple. You should be able to reuse the
    functions you’ve already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  /* Exercise 6.4
    Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def go(count: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
      if count == 0 then (acc, r)
      else
        val (i, r1) = r.nextInt
        go(count - 1, r1, i :: acc)
    go(count, rng, List())

  // type alias for a state transition
  // (function that you pass a random generator, and generates the next number and state)
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = rng => rng.nextInt

  /* a simple RNG state transition is the unit action, which passes the RNG state
   through without using it, always returning a constant value. */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /* map transforms the output of a state action without further modifying the resultant state.
  Remember, Rand[A] is just a type alias for a function type RNG => (A, RNG),
  so this is just a kind of function composition: */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  /* Exercise 6.5
    Use map to reimplement double in a more succinct way. See exercise 6.2.
   */
  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1.0)

  /*
   Exercise 6.6
    Write the implementation of map2 based on the following signature. This
    function takes two actions, ra and rb, and a function f for combining their
    results, and returns a new action that combines them:
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  /* We only have to write the map2 function once, and then we can use it to
  combine arbitrary RNG state actions. For example, if we have an action that
  generates values of type A and an action to generate values of type B, then we
  can combine them into one action that generates pairs of both A and B: */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /* Exercise 6.7
    Implement sequence for combining a List of actions into a single action.
    Use it to reimplement the ints function you wrote before. For the latter,
    you can use the standard library function List.fill(n)(x) to
    make a list with x repeated n times.
   */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod       = i % n
      if i + (n - 1) - mod >= 0 then (mod, rng2)
      else nonNegativeLessThan(n)(rng2)

  /* Exercise 6.8
    Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)
    }

  /* Exericse 6.9
    Reimplement map and map2 in terms of flatMap. The fact that this is possible
    is what we’re referring to when we say that flatMap is more powerful than
    map and map2.
   */

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  /* Can we make a more testable die roll using our purely functional API? */
  def rollDie: Rand[Int] =
    map(nonNegativeLessThan(6))(_ + 1)

// type State[S, +A] = S => (A, S)
// case class State[S, +A](run: S => (A, S))

/* An opaque type behaves like a type alias inside the defining scope.
  Outside of the defining scope though, the opaque type is unrelated to the
  representation type. Here, "defining scope" refers to the object containing the
  definition, or if the definition is top level, the package containing it
 */
opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    /* Exercise 6.10
      Generalize the functions unit, map, map2, flatMap, and sequence. Add them
      as extension methods on the State type where possible. Otherwise you
      should put them in the State companion object.
     */

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      // underlying.flatMap(a => sb.map(b => f(a, b)))
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  // allows State to be constructed from a function
  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get       // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  // get, set, unit, map, map2  and flatMap are all the tooling needed
  // to implement any kind of state machine or stateful program in a functional way.

/* Exercise 6.11 (HARD)
  Implement a finite state automaton that models a simple candy dispenser.
  The machine has two types of input: you can insert a coin, or you can turn
  the knob to dispense candy. It can be in one of two states: locked or unlocked.
  It also tracks how many candies are left and how many coins it contains.

  The rules of the machine are as follows:
  - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  - A machine that’s out of candy ignores all inputs.
 */

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser:
  import Input.*

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      // _ <- State.sequence(inputs.map(i => State.modify(update(i))))
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  private def update(input: Input)(state: Machine): Machine = (input, state) match
    case (_, m @ Machine(_, 0, _))              => m
    case (Coin, m @ Machine(false, _, _))       => m
    case (Turn, m @ Machine(true, _, _))        => m
    case (Coin, Machine(true, candies, coins))  => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
