package functorial

trait MonadOr[F[+_]] extends Monad[F]
                        with ApplicativeOr[F] { module => 
  override implicit def syntax[A](m: F[A]): MonadOr.Syntax[F,A]
                                      = new MonadOr.Syntax[F,A] {
    val F = module
    def self = m
  }
}

object MonadOr {
  trait Syntax[F[+_],+A] 
    extends Monad.Syntax[F,A]
       with ApplicativeOr.Syntax[F,A]
       with HasCompanion[MonadOr[F]]
}
