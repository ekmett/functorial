package functorial

trait MonadReader[F[+_],E] extends Monad[F] with PointedReader[F,E] { module => 
  override implicit def syntax[A](m: F[A]): MonadReader.Syntax[F,E,A] = new MonadReader.Syntax[F,E,A] {
    val F = module
    def self = m
  }
}

object MonadReader {
  trait Syntax[F[+_],E,+A] extends Monad.Syntax[F,A] with HasCompanion[MonadReader[F,E]]
}

