package functorial

import scala.collection.generic

trait Monad[F[+_]] extends Applicative[F] with Bind[F] { module => 
  def apply[A,B](f: A => B, a: F[A]): F[B] = a flatMap (a0 => pure(f(a0)))
  def ap[A,B](f: F[A => B], m: F[A]): F[B] = f flatMap (m map)
  def when(cond: Boolean, s: F[Unit]): F[Unit] = if (cond) s else unit
  def unless(cond: Boolean, s: F[Unit]): F[Unit] = if (cond) unit else s 
  override implicit def syntax[A](m: F[A]): Monad.Syntax[F,A] = new Monad.Syntax[F,A] {
    val companion: Monad[F] = module
    def value: F[A] = m
  }
}

object Monad { 
  trait Syntax[F[+_],+A] extends Applicative.Syntax[F,A] with Bind.Syntax[F,A] with HasCompanion[Monad[F]]
  trait Transformer [T[_[+_],+_]] extends Companion {
    def lift[M[+_]:Monad,A](m : M[A]): T[M,A]
  }
  object Transformer {
    trait Syntax[T[_[+_],+_],M[+_],+A] extends HasCompanion[Monad.Transformer[T]] {
      val underlyingMonad: Monad[M]
    }
  }
}

