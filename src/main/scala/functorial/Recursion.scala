package functorial

class Nu[+F[+_]](f: => F[Nu[F]]) { 
  def out: F[Nu[F]] = f
}

object Nu {
  def apply[F[+_]](f: => F[Nu[F]]) = new Nu[F](f)
}

class Mu[+F[+_]](f: F[Mu[F]]) extends Nu[F](f) { 
  override val out: F[Mu[F]] = f
}

object Mu { 
  def apply[F[+_]](f: F[Mu[F]]) = new Mu[F](f)
}

object Recursion { 
  def cata[F[+_],A](m: Mu[F])(phi: F[A] => A)(implicit F:Functor[F]): A = 
    phi(F(m.out)(cata(_)(phi)))
  def ana[F[+_],A](a: A)(psi: A => F[A])(implicit F:Functor[F]): Nu[F] =
    new Nu[F](F(psi(a))(ana(_)(psi)))
  def hylo[F[+_],A,B](a: A)(psi: A => F[A])(phi: F[B] => B)(implicit F:Functor[F]): B =
    phi(F(psi(a))(hylo(_)(psi)(phi)))
}
