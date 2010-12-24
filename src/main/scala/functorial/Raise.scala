package functorial

trait Raise[F[+_],E] extends Or[F] { module => 
  def raise(e: E): F[Nothing]
  def handle[A](a: F[A])(b: E => F[A]): F[A]
  def or[A](a: F[A], b: => F[A]): F[A] = handle(a)(_ => b)
  override implicit def syntax[A](m: F[A]): Raise.Syntax[F,E,A] = new Raise.Syntax[F,E,A] {
    val F = module
    def value = m
  }
}

object Raise {
  trait Syntax[F[+_],E,+A] extends Or.Syntax[F,A] 
                              with HasCompanion[Raise[F,E]] { m => 
    def handling[B>:A](b: E => F[B]): F[B] = F.handle[B](value)(b)
  }
}
