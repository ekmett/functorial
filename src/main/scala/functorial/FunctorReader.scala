package functorial

trait FunctorReader[F[+_],E] extends Functor[F] {
  def ask: F[E] = reader[E](x => x)
  def reader[X](f: E => X): F[X]
}

