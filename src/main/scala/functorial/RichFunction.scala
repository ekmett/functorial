package functorial

import scala.collection.generic

// richer functions to enable a more haskelly ordering of events
class RichFunction1[-A,+B](f: A => B) { 
  def !!![F[+_]:Functor](a: F[A]): F[B] = implicitly[Functor[F]].apply(f, a)
  def !![F[+_]:Functor](a: F[A]): F[B] = implicitly[Functor[F]].apply(f, a)
  def !(a: A): B = f(a)
}

trait RichFunctions { 
  implicit def rich1[A,B](f: A => B): RichFunction1[A,B] = new RichFunction1[A,B](f)
  implicit def rich2[A,B,C](f: (A,B) => C): RichFunction2[A,B,C] = new RichFunction2[A,B,C](f)
  implicit def rich3[A,B,C,D](f: (A,B,C) => D): RichFunction3[A,B,C,D] = new RichFunction3[A,B,C,D](f)
  implicit def rich4[A,B,C,D,E](f: (A,B,C,D) => E): RichFunction4[A,B,C,D,E] = new RichFunction4[A,B,C,D,E](f)
}

class RichFunction2[-A,-B,+C](f: (A,B) => C) { 
  def !!![F[+_]:Applicative](a: F[A], b: F[B]): F[C] = implicitly[Applicative[F]].lift2(a,b)(f)
  def !![F[+_]:Applicative](a: F[A]): F[B => C] = implicitly[Applicative[F]].apply(f.curried, a)
  def !(a: A): B => C = b => f(a,b)
}

class RichFunction3[-A,-B,-C,+D](f: (A,B,C) => D) { 
  def !!![F[+_]:Applicative](a: F[A], b: F[B], c: F[C]): F[D] = implicitly[Applicative[F]].lift3(a,b,c)(f)
  def !![F[+_]:Applicative](a: F[A]): F[(B,C) => D] = implicitly[Applicative[F]].apply((a:A) => (b,c) => f(a,b,c), a)
  def !(a: A): (B,C) => D = (b,c) => f(a,b,c)
}

class RichFunction4[-A,-B,-C,-D,+E](f: (A,B,C,D) => E) { 
  def !!![F[+_]:Applicative](a: F[A], b: F[B], c: F[C], d: F[D]): F[E] = implicitly[Applicative[F]].lift4(a,b,c,d)(f)
  def !![F[+_]:Applicative](a: F[A]): F[(B,C,D) => E] = implicitly[Applicative[F]].apply((a:A) => (b,c,d) => f(a,b,c,d), a)
  def !(a: A): (B,C,D) => E = (b,c,d) => f(a,b,c,d)
}

