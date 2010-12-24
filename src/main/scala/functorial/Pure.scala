package functorial

trait Pure[F[+_]] extends Companion { module => 
  def pure[A](a: A): F[A]
  def unit: F[Unit] = pure[Unit](())
  implicit def syntax[A](m: F[A]): Pure.Syntax[F,A] 
                             = new Pure.Syntax[F,A] {
    val F = module
    def value = m
  }
}

object Pure { 
  trait Syntax[F[+_],+A] extends HasCompanion[Pure[F]]
                            with Wrapped[F[A]]
}

