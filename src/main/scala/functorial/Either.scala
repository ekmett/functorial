package functorial

object Either {
  class monadOr[E] extends MonadOr[({type λ[+X] = Either[E,X]})#λ] {
    def pure[A](a: A): Either[E,A] = Right(a)
    def bind[A,B](a: Either[E,A])(f: A => Either[E,B]): Either[E,B] = a match { 
      case left : Left[_,_] => left.asInstanceOf[Either[E,B]] 
      case Right(x) => f(x)
    }
    def or[A](x: Either[E,A], y: => Either[E,A]): Either[E,A] = x match { 
      case _ : Left[_,_] => y
      case _ : Right[_,_] => x
    }
  }
  class monadPlus[E](default: => E)
       extends monadOr[E] with MonadPlus[({type λ[+X] = Either[E,X]})#λ] {
    def empty = Left(default)
  }
  def monad[E] = new monadOr[E]
  def monad[E](default: E) = new monadPlus[E](default)
}
