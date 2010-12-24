package functorial

trait PointedReader[F[+_],E] extends Pointed[F] {
  def pure[A](a: A): F[A] = reader[A](_ => a)
  def ask: F[E] = reader[E](x => x)
  def reader[X](f: E => X): F[X]
}
