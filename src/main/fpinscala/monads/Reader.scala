package fpinscala.monads

opaque type Reader[-R, A] = R => A

object Reader:
  def ask[R]: Reader[R, R] = r => r

  def apply[R, A](f: R => A): Reader[R, A] = f

  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  // given readerMonad[R]: Monad[Reader[R, _]] with
  //   def unit[A](a: => A): Reader[R, A] = _ => a
  //   extension [A](fa: Reader[R, A])
  //     override def flatMap[B](f: A => Reader[R, B]) =
  //       r => f(fa(r))(r)

