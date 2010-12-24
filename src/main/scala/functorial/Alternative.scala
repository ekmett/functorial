package functorial

import scala.collection.generic

trait Alternative[F[+_]] extends Empty[F]
                            with Or[F]
                            with Applicative[F] { module => 
  def many[A](m: F[A]): F[List[A]] = {
    lazy val many_m : F[List[A]] = lift2(m, many_m)(_ :: _)
    many_m
  }
  def some[A](m: F[A]): F[List[A]] = lift2(m, many(m))(_ :: _)
  def optional[A](m: F[A]): F[Option[A]] = (m map (Some(_):Option[A])) | pure(None)
  override implicit def syntax[A](m: F[A]): Alternative.Syntax[F,A] = new Alternative.Syntax[F,A] {
    val F = module
    def value = m
  }
}
object Alternative {
  trait Syntax[F[+_],+A] extends Empty.Syntax[F,A]
                            with Or.Syntax[F,A]
                            with Applicative.Syntax[F,A] 
                            with HasCompanion[Alternative[F]] { m => 
    def * : F[List[A]] = F.many(m)
    def + : F[List[A]] = F.some(m)
    def ? : F[Option[A]] = F.optional(m)
  }
}

