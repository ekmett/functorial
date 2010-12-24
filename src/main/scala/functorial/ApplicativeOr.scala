package functorial

trait ApplicativeOr[F[+_]] extends Applicative[F]
                              with Or[F] { module => 
  def many[A](m: F[A]): F[List[A]] = {
    lazy val many_m : F[List[A]] = lift2(m, many_m)(_ :: _)
    many_m
  }
  def some[A](m: F[A]): F[List[A]] = lift2(m, many(m))(_ :: _)
  def optional[A](m: F[A]): F[Option[A]] = (m map (Some(_):Option[A])) | pure(None)
  override implicit def syntax[A](m: F[A]): ApplicativeOr.Syntax[F,A]
                                      = new ApplicativeOr.Syntax[F,A] {
    val F = module
    def value = m
  }
}

object ApplicativeOr {
  trait Syntax[F[+_],+A] 
    extends Applicative.Syntax[F,A]
       with Or.Syntax[F,A]
       with HasCompanion[ApplicativeOr[F]] {
    def * : F[List[A]] = F.many(value)
    def + : F[List[A]] = F.some(value)
    def ? : F[Option[A]] = F.optional(value)
  }
}
