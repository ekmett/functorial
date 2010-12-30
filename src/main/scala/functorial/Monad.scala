package functorial

import scala.collection.generic

trait Monad[F[+_]] extends Applicative[F] with Bind[F] { module => 
  override def apply[A,B](a: F[A])(f: A => B): F[B] = a flatMap (a0 => pure(f(a0)))
  def ap[A,B](m: F[A])(f: F[A => B]): F[B] = f flatMap (m map)
  def when(cond: Boolean, s: F[Unit]): F[Unit] = if (cond) s else unit
  def unless(cond: Boolean, s: F[Unit]): F[Unit] = if (cond) unit else s 
  override implicit def syntax[A](m: F[A]): Monad.Syntax[F,A] = new Monad.Syntax[F,A] {
    val F: Monad[F] = module
    def self = m
  }
}

object Monad { 
  trait Syntax[F[+_],+A] extends Applicative.Syntax[F,A] with Bind.Syntax[F,A] with HasCompanion[Monad[F]]
  trait Transformer [T[_[+_],+_]] extends Companion { trans => 
    def lift[M[+_]:Monad,A](m : M[A]): T[M,A]
/*
    def get[M[+_],S](implicit M:MonadState[M,S]): T[M,S] = lift(M.get)
    def gets[M[+_],S,A](f: S => A)(implicit M:MonadState[M,S]): T[M,A] = lift(M.gets(f))
    def state[M[+_],S,A](f: S => (A,S))(implicit M:MonadState[M,S]): T[M,S] = lift(M.state(f))
    def ask[M[+_],E](implicit M:MonadReader[M,E]): T[M,E] = lift(M.ask)
    def reader[M[_],E,A](f: E => A)(implicit M:MonadReader[M,E]): T[M,A] = lift(M.reader(f))
    def liftReader[M[+_],E](implicit M:Monad[M],R:PointedReader[M,E]): PointedReader[({type λ[+X] = T[M,X]})#λ] 
                                                                 = new PointedReader[({type λ[+X] = T[M,X]})#λ] {
      def reader[A](f: E => A): T[M,A] = lift(M.reader(f))
    }
    def liftState[M[+_],S](implicit M:Monad[M],S:PointedState[M,S]): PointedState[({type λ[+X] = T[M,X]})#λ] 
                                                               = new PointedState[({type λ[+X] = T[M,X]})#λ] {
      def state[A](f: S => (A,S)): T[M,A] = lift(M.state(f))
    }
    implicit def asReader[M[+_],E](T : this.type)(implicit M:Monad[M], R:PointedReader[M,E]): PointedReader[({type λ[+X] = T[M,X]})#λ,E] = readerT[M,E](M,R)
    implicit def asState[M[+_],S](T : this.type)(implicit M:Monad[M], S:PointedState[M,S]): PointedState[({type λ[+X] = T[M,X]})#λ,S] = readerT[M,S](M,S)
*/
  }
  object Transformer {
    trait Syntax[T[_[+_],+_],M[+_],+A] extends HasCompanion[Monad.Transformer[T]] {
      val M: Monad[M]
    }
  }
}

