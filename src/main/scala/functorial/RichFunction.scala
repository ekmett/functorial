package functorial

class RichFunction1[-A,+B](f: A => B) { 
  def !!![F[+_]](a: F[A])(implicit F:Functor[F]): F[B] = F(a)(f)
  def !![F[+_]](a: F[A])(implicit F:Functor[F]): F[B] = F(a)(f)
  def !(a: A): B = f(a)
}

class RichFunction2[-A,-B,+C](f: (A,B) => C) { 
  def !!![F[+_]](a: F[A], b: F[B])(implicit F:Applicative[F]): F[C] = F.lift2(a,b)(f)
  def !![F[+_]](a: F[A])(implicit F:Applicative[F]): F[B => C] = F(a)(f.curried)
  def !(a: A): B => C = b => f(a,b)
}

class RichFunction3[-A,-B,-C,+D](f: (A,B,C) => D) { 
  def !!![F[+_]](a: F[A], b: F[B], c: F[C])(implicit F:Applicative[F]): F[D] = F.lift3(a,b,c)(f)
  def !![F[+_]](a: F[A])(implicit F:Applicative[F]): F[(B,C) => D] = F(a)((a:A) => (b,c) => f(a,b,c))
  def !(a: A): (B,C) => D = (b,c) => f(a,b,c)
}

class RichFunction4[-A,-B,-C,-D,+E](f: (A,B,C,D) => E) { 
  def !!![F[+_]](a: F[A], b: F[B], c: F[C], d: F[D])(implicit F:Applicative[F]): F[E] = F.lift4(a,b,c,d)(f)
  def !![F[+_]](a: F[A])(implicit F:Applicative[F]): F[(B,C,D) => E] = F(a)((a:A) => (b,c,d) => f(a,b,c,d))
  def !(a: A): (B,C,D) => E = (b,c,d) => f(a,b,c,d)
}

trait RichFunctions { 
  implicit def rich1[A,B](f: A => B) = new RichFunction1[A,B](f)
  implicit def rich2[A,B,C](f: (A,B) => C) = new RichFunction2[A,B,C](f)
  implicit def rich3[A,B,C,D](f: (A,B,C) => D) = new RichFunction3[A,B,C,D](f)
  implicit def rich4[A,B,C,D,E](f: (A,B,C,D) => E) = new RichFunction4[A,B,C,D,E](f)
}
