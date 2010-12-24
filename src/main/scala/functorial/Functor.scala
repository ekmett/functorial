package functorial

trait Functor[F[+_]] extends Companion { module => 
  def apply[A,B](f: A => B, a: F[A]): F[B]
  implicit def syntax[A](m: F[A]): Functor.Syntax[F,A] = new Functor.Syntax[F,A] {
    val companion = module
    def value = m
  }
}

object Functor { 
  trait Syntax[F[+_],+A] extends HasCompanion[Functor[F]] with Wrapped[F[A]] {
    def map[B](f: A => B): F[B] = companion.apply(f, value)
  }
}
