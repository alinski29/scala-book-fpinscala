package fpinscala.iomonad

import scala.io.StdIn.readLine
import fpinscala.iomonad.Monad.*
import fpinscala.iomonad.Monad

object IO1:

  trait IO[A]:
    self =>

    def unsafeRun: A

    def map[B](f: A => B): IO[B] = new:
      def unsafeRun = f(self.unsafeRun)

    def flatMap[B](f: A => IO[B]): IO[B] = new:
      def unsafeRun = f(self.unsafeRun).unsafeRun

  object IO:

    def apply[A](a: => A): IO[A] = new:
      def unsafeRun = a

    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]) =
          fa.flatMap(f)

    def ref[A](a: A): IO[IORef[A]] = IO(new IORef(a))
    final class IORef[A](private var value: A):
      def set(a: A): IO[A]         = IO { value = a; a }
      def get: IO[A]               = IO(value)
      def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  // def empty: IO = new {
  //   def unsafeRun = ()
  // }

  def ReadLine: IO[String]             = IO(readLine())
  def PrintLine(msg: String): IO[Unit] = IO(println(msg))
  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  def converter: IO[Unit] =
    for
      _ <- PrintLine("Enter a temperature degrees in Fahrenheit")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    yield ()

  /*                         Some other examples                      */

  // An `IO[Unit]` that reads a line from the console and echoes it back.
  val echo = ReadLine.flatMap(PrintLine)

  // Parses an `Int` by reading a line from the console.
  val readInt: IO[Int] = ReadLine.map(_.toInt)

  // Parses an `(Int,Int)` by reading two lines from the console.
  val readInts: IO[(Int, Int)] = readInt ** readInt

  // Repeat `converter` 5 times, discarding the results (which are
  // just `Unit`). We can replace `converter` here with any `IO`
  // action we wished to repeat 5 times (ex: `echo` or `readInts`).
  val prompts: IO[Unit] = converter.replicateM_(5)

  // An `IO[List[String]]` that will read 10 lines from the console and
  // return the list of results.
  val lines: IO[List[String]] = ReadLine.replicateM(10)

  val helpstring = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin
  // import IO.m

  // def factorial(n: Int): IO[Int] =
  //   for
  //     acc    <- IO.ref(1)
  //     _      <- foreachM(1 to n to (LazyList))(i => acc.modify(_ * i).void)
  //     result <- acc.get
  //   yield result

  // val factorialREPL: IO[Unit] = sequence_(
  //   PrintLine(helpstring),
  //   ReadLine.doWhile { line =>
  //     val ok = line != "q"
  //     when(ok) {
  //       for
  //         n <- factorial(line.toInt)
  //         _ <- PrintLine("factorial: " + n)
  //       yield ()
  //     }
  //   }
  // )

end IO1

object IO2a:

  enum IO[A]:
    case Return(a: A)             // an IO action that has finished
    case Suspend(resume: () => A) // we want to execute some kind of effect to produce a result
    // lets us extend or continue a computation by using the result of the first computation to producea new computation
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value

    def map[B](f: A => B): IO[B] =
      flatMap(a => Return(f(a)))

    // There is only one sensible way to implement this as a
    // tail-recursive function, the one tricky case is left-nested
    // flatMaps, as in `a.flatMap(f).flatMap(g)`, which we
    // reassociate to the right as `a.flatMap(ar => f(a).flatMap(g))`
    @annotation.tailrec
    final def unsafeRun(): A = this match
      case Return(a)  => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match
          case Return(a)     => f(a).unsafeRun()
          case Suspend(r)    => f(r()).unsafeRun()
          case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun()

  object IO:
    def apply[A](a: => A): IO[A] =
      suspend(Return(a))

    def suspend[A](ioa: => IO[A]): IO[A] =
      Suspend(() => ioa).flatMap(identity)

    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]): IO[B] =
          fa.flatMap(f)

    def printLine(s: String): IO[Unit] =
      IO.Suspend(() => println(s))

    import IO.monad.*

    val p: IO[Unit] = printLine("Still going...").forever

    val actions: LazyList[IO[Unit]] =
      LazyList.fill(100000)(printLine("Still going..."))

    val composite: IO[Unit] =
      actions.foldLeft(IO(()))((acc, a) => acc.flatMap(_ => a))

end IO2a

object IO2b:

  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  enum TailRec[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] =
      flatMap(a => Return(f(a)))

    @annotation.tailrec
    final def run: A = this match
      case Return(a)  => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match
          case Return(a)     => f(a).run
          case Suspend(r)    => f(r()).run
          case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).run

  object TailRec:
    def apply[A](a: => A): TailRec[A] =
      suspend(Return(a))

    def suspend[A](a: => TailRec[A]) =
      Suspend(() => a).flatMap(identity)

    given monad: Monad[TailRec] with
      def unit[A](a: => A): TailRec[A] =
        TailRec(a)
      extension [A](fa: TailRec[A])
        def flatMap[B](f: A => TailRec[B]): TailRec[B] =
          fa.flatMap(f)

end IO2b

object IO2c:

  import fpinscala.parallelism.Nonblocking.*

  enum Async[A]:
    case Return(a: A)
    case Suspend(resume: Par[A])
    case FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(a => Return(f(a)))

      // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
    @annotation.tailrec
    final def step: Async[A] =
      this match
        case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
        case FlatMap(Return(x), f)     => f(x).step
        case _                         => this

    def run: Par[A] = step match
      case Return(a)  => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) =>
        x match
          case Suspend(r) => r.flatMap(a => f(a).run)
          case _          => sys.error("Impossible, since `step` eliminates these cases")

    object Async:
      def apply[A](a: => A): Async[A] =
        Return(a)

      def suspend[A](aa: => Async[A]): Async[A] =
        Suspend(Par.delay(aa)).flatMap(identity)

      given monad: Monad[Async] with
        def unit[A](a: => A): Async[A] =
          Return(a)
        extension [A](fa: Async[A])
          def flatMap[B](f: A => Async[B]): Async[B] =
            fa.flatMap(f)

end IO2c

object IO3:

  /*
  We can generalize `TailRec` and `Async` to the type `Free`, which is
  a `Monad` for any choice of `F`.
   */

  enum Free[F[_], A]:
    case Return(a: A)
    case Suspend(s: F[A])
    case FlatMap[F[_], A, B](
        s: Free[F, A],
        f: A => Free[F, B]
    ) extends Free[F, B]

    /* Exercise 13.3
    Hard: Implement a generic interpreter for Free[F, A], given a Monad[F].
    Define this interpreter as a method on the Free type. You can pattern your
    implementation after the Async interpreter given previously, including use of
    a tail-recursive step function.
     */
    def run(using F: Monad[F]): F[A] =
      step match
        case Return(a)               => F.unit(a)
        case Suspend(fa)             => fa
        case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
        case FlatMap(_, _)           => sys.error("Impossible, since `step` eliminates these cases")

    @annotation.tailrec
    final def step: Free[F, A] =
      this match
        case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(g)).step
        case FlatMap(Return(x), f)      => f(x).step
        case _                          => this

  // type TailRec[A] = Free[Function0, A]
  // type Async[A] = Free[Par, A]

  object Free:

    /* Exercise 13.1
     Free is a monad for any choice of F. Implement map and flatMap methods on
    the Free enum, and give the Monad instance for Free[F,_].
     */
    given FreeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
      def unit[A](a: => A): Free[F, A] =
        Return(a)

      extension [A](fa: Free[F, A])
        def flatMap[B](f: A => Free[F, B]) =
          fa.flatMap(f)

    /* Exercise 13.2
    Implement a specialized tail-recursive interpreter, runTrampoline, for
    running a Free[Function0, A].
     */
    extension [A](fa: Free[Function0, A])
      @annotation.tailrec
      def runTrampoline: A =
        fa match
          case Return(a)   => a
          case Suspend(ta) => ta()
          case FlatMap(fx, f) =>
            fx match
              case Return(x)      => f(x).runTrampoline
              case Suspend(tx)    => f(tx()).runTrampoline
              case FlatMap(fy, g) => fy.flatMap(y => g(y).flatMap(f)).runTrampoline

end IO3
