package functorial

import scala.collection.generic

trait Filter[F[+_]] extends Companion { module => 
  def filter[A](f: F[A], p: A => Boolean): F[A]
}
object Filter {
  trait Syntax[F[+_],+A] extends Wrapped[F[A]] with HasCompanion[Filter[F]] { m => 
    def filter(p: A => Boolean): F[A] = companion.filter(m, p)
  }
}

