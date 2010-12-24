package functorial

object Identity extends Monad[({type λ[+X]=X})#λ] {
  def pure[A](a: A) = a
  def bind[A,B](m: A)(f: A => B): B = f(m)
  def map[A,B](m: A)(f: A => B): B  = f(m)
}
