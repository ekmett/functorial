package functorial

import scala.collection.generic

trait Alternative[F[+_]] extends Empty[F]
                            with ApplicativeOr[F] { module => 
  override implicit def syntax[A](m: F[A]): Alternative.Syntax[F,A] = new Alternative.Syntax[F,A] {
    val F = module
    def self = m
  }
}
object Alternative {
  trait Syntax[F[+_],+A] extends Empty.Syntax[F,A]
                            with ApplicativeOr.Syntax[F,A] 
                            with HasCompanion[Alternative[F]]
}

