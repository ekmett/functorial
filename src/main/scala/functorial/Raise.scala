package functorial

trait Raise[F[+_],E] extends Companion { module => 
  def raise(e: E): F[Nothing]
  def handle[A](a: F[A])(b: E => F[A]): F[A]
  implicit def syntax[A](m: F[A]): Raise.Syntax[F,E,A] = new Raise.Syntax[F,E,A] {
    val F = module
    def value = m
  }
}

object Raise {
  trait Syntax[F[+_],E,+A] extends HasCompanion[Raise[F,E]] 
                              with Wrapped[F[A]] { m => 
    def handling[B>:A](b: E => F[B]): F[B] = F.handle[B](value)(b)
  }
}
