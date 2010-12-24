package functorial
/*

sealed case class EitherT[M[+_],E,+A](
  run: M[Either[E,A]]
) (
  implicit val M:Monad[M]
) extends Monad.Syntax[({type λ[+X] = EitherT[M,E,X]})#λ, A]
     with Monad.Transformer.Syntax[({type λ[N[+_],+B] = EitherT[N,E,B]})#λ,M,A] 
     with HasCompanion[EitherT.monadOr[M,E]]{
  final def value = this
  final val F = new EitherT.monadOr[M,E]
  final override def flatMap[B](k: A => EitherT[M,E,B]) = new EitherT[M,E,B](run flatMap { 
    case l: Left[_,_] => pure(l.asInstanceOf[Either[E,B]])
    case Right(a) => k(a)
  })
  final override def map[B](k: A => B) = new EitherT[M,E,B](run map {
    case l: Left[_,_] => l.asInstanceOf[Either[E,B]]
    case Right(a) => k(a)
  })
  final def |[B>:A](n: EitherT[M,E,B]) = new EitherT[M,E,B](run flatMap { 
    case _ : Left[_,_] => n.run
    case r => M.pure(r)
  })
}
        
object EitherT {
  class monadOr[M[+_],E](
    implicit val M:Monad[M]
  ) extends MonadOr[({type λ[+X] = EitherT[E,X]})#λ] {
    import M._
    def pure[A](a: A): EitherT[M,E,A] = new EitherT[M,E,A](pure(Right(a)))
    def bind[A,B](m: EitherT[M,E,B])(k: A => EitherT[M,E,B]) = m flatMap k
    def apply[A,B](m: EitherT[M,E,B])(k: A => B) = m map k
    def or[A](x: EitherT[M,E,A], y: => EitherT[M,E,A]) = x | y
  }
  class monadPlus[M[+_],E](default: => E)(implicit M:Monad[M])
       extends monadOr[M,E](M) with MonadPlus[({type λ[+X] = Either[E,X]})#λ] {
    def empty = new EitherT[M,E,B](M.pure(Left(default)))
  }
  def monad[E] = new monadOr[E]
  def monad[E](default: E) = new monadPlus[E](default)
}
*/
