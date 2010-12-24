package functorial

import scala.collection.generic

trait Filter[F[+_]] extends Companion { module => 
  def filter[A](f: F[A], p: A => Boolean): F[A]
  implicit def syntax[A](m: F[A]): Filter.Syntax[F,A] = new Filter.Syntax[F,A] {
    val F = module
    def value = m
  }
}
object Filter {
  trait Syntax[F[+_],+A] extends Wrapped[F[A]] with HasCompanion[Filter[F]] { m => 
    def filter(p: A => Boolean): F[A] = F.filter(m, p)
  }
}

