package functorial

trait Functor[F[+_]] extends Companion { module => 
  def apply[A,B](a: F[A])(f: A => B): F[B]
  implicit def syntax[A](m: F[A]): Functor.Syntax[F,A] = new Functor.Syntax[F,A] {
    val F = module
    def value = m
  }
}

object Functor { 
  trait Syntax[F[+_],+A] extends HasCompanion[Functor[F]] with Wrapped[F[A]] {
    def map[B](f: A => B): F[B] = F(value)(f)
  }
}
