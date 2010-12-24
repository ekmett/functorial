package functorial

import scala.collection.generic

trait ContravariantFunctor[F[-_]] extends Companion { module => 
  def apply[A,B](f: B => A, a: F[A]): F[B]
  implicit def syntax[A](m: F[A]): ContravariantFunctor.Syntax[F,A] = new ContravariantFunctor.Syntax[F,A] {
    val companion = module
    def value = m
  }
}

object ContravariantFunctor {
  trait Syntax[F[-_],-A] extends HasCompanion[ContravariantFunctor[F]] with Wrapped[F[A]] {
    def map[B](f: B => A): F[B] = companion.apply(f, value)
  }
}

