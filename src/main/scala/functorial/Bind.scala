package functorial

import scala.collection.generic

trait Bind[F[+_]] extends Companion { module => 
  def bind[A,B](f: A => F[B], m: F[A]): F[B]
  def join[A](f: F[F[A]]): F[A] = bind(identity[F[A]], f)
  implicit def syntax[A](m: F[A]): Bind.Syntax[F,A] =
                                        new Bind.Syntax[F,A] {
    val companion = module
    def value = m
  }
}

object Bind {
  trait Syntax[F[+_],+A] extends HasCompanion[Bind[F]] with Wrapped[F[A]] { m => 
    def flatMap[B](f: A => F[B]): F[B] = companion.bind(f,m)
  }
}

