package fpinscala.iomonad

import fpinscala.TestSpec

class DerivingIOSpec extends TestSpec:

  "Derving IO" should {

    "IO2a tests" in {
      import IO2a.IO

      val ioNums: LazyList[IO[Int]] = LazyList((1 to 100)*).map(IO(_))
      val ioMaxNum: IO[Int] = ioNums.foldLeft(IO(0))((a, b) =>
        for
          aa <- a
          bb <- b
        yield if aa >= bb then aa else bb
      )

      ioMaxNum.unsafeRun() shouldEqual 100
    }

    "Tailrec (IO2b) tests" in {
      import IO2b.TailRec
      val f: Int => TailRec[Int] = (i: Int) => TailRec.Return(i)

      val g: Int => TailRec[Int] =
        List.fill(10000)(f).foldLeft(f)((a, b) => x => TailRec.suspend(a(x).flatMap(b)))

      g(42).run shouldEqual 42
    }

  }
