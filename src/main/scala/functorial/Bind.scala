package functorial

import scala.collection.generic

trait Bind[F[+_]] extends Companion { module => 
  def bind[A,B](m: F[A])(f: A => F[B]): F[B]
  def join[A](f: F[F[A]]): F[A] = bind(f)(identity[F[A]])
  implicit def syntax[A](m: F[A]): Bind.Syntax[F,A] = new Bind.Syntax[F,A] {
    val F = module
    def value = m
  }
}

object Bind {
  trait Syntax[F[+_],+A] extends HasCompanion[Bind[F]] with Wrapped[F[A]] { m => 
    def flatMap[B](f: A => F[B]): F[B] = F.bind(m)(f)
  }
}

