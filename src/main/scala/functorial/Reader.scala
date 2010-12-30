package functorial

sealed class Reader[E,+A](val self: E => A) 
     extends Monad.Syntax[({type λ[+X] = E => X})#λ, A]
        with HasCompanion[Reader.monad[E]] { m => 
  final def apply(e: E): A = self(e)
  final val F = new Reader.monad[E]
  final override def flatMap[B](g: A => E => B): E => B = e => g(self(e))(e)
  final override def map[B](f: A => B): E => B = e => f(self(e))
}

object Reader {
  implicit def apply[E,A](f: E => A) = new Reader[E,A](f)
  def monad[E]: monad[E] = new monad[E]
  sealed class monad[E] 
       extends Monad[({type λ[+X] = E => X})#λ] 
          with PointedReader[({type λ[+X] = E => X})#λ,E] {
    override def pure[A](a: A): Any => A = _ => a
    def bind[A,B](m: E => A)(f: A => E => B): E => B = e => f(m(e))(e)
    def reader[A](f: E => A): E => A = f
  }
}

