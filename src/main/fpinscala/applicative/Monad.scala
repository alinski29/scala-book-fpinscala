package fpinscala.applicative

trait Monad[F[_]] extends Applicative[F]:

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)
