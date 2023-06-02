package fpinscala.monads

import fpinscala.TestSpec
import fpinscala.monads.Monad
import fpinscala.monads.Monad.*
import fpinscala.monads.Monad.given
import fpinscala.parallelism.Nonblocking.*
import fpinscala.state.*
import fpinscala.testing.*

class MonadSpec extends TestSpec:

  "Monads" should {

    import Prop.Result

    "test the behavior of State monad" in { 

      val getAndIncrement: State[Int, Int] = 
        for
          i <- State.get
          _ <- State.set(i + 1)
        yield i

      getAndIncrement.run(0) shouldEqual (0, 1)
      getAndIncrement.map2(getAndIncrement)((_, _)).run(0) shouldEqual ((0, 1), 2)

    }
  }
end MonadSpec

// object MonadSpec:
//   
//   
//   private def assertMonad[F[_]](tm: TestedMonad[F[_]], n: Int, s: String): Unit = ???

//   private def assertUnit[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
//     import tm.*
//     assertFs(monad.unit(n), pure(n))

//   private trait TestedMonad[F[_]]:
//     val monad: Monad[F]
//     def pure[A]: A => F[A]
//     def assertFs[A](actual: F[A], expected: F[A]): Unit =
//       actual shouldEqual expected
//     //   Assertions.assertEquals(actual, expected)
//     def f: List[Int] => F[Int] = list => pure(list.sum)
//     def g: Int => F[String] = i => pure(i.toString)
//     def h: String => F[Boolean] = s => pure(s.startsWith("-"))

//   private val optionMonad: TestedMonad[Option] = 
//     new TestedMonad[Option] {
//       val monad: Monad[Option] = Monad.optionMonad
//       def pure[A]: A => Option[A] = Some.apply
//     }

// end MonadSpec
