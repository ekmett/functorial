package functorial

sealed class EitherT[M[+_],E,+A](val run: M[Either[E,A]])(implicit val M:Monad[M]) 
     extends MonadOr.Syntax[({type λ[+X] = EitherT[M,E,X]})#λ, A]
        with Monad.Transformer.Syntax[({type λ[N[+_],+B] = EitherT[N,E,B]})#λ,M,A]
        with Raise.Syntax[({type λ[+X] = EitherT[M,E,X]})#λ,E,A]
        with HasCompanion[EitherT.monadOr[M,E]] {
  import M._
  final def value = this
  final val F : EitherT.monadOr[M,E] = new EitherT.monadOr[M,E](M)
  final override def flatMap[B](k: A => EitherT[M,E,B]): EitherT[M,E,B] = new EitherT[M,E,B](run flatMap { 
    case l : Left[_,_] => M.pure(l.asInstanceOf[Either[E,B]])
    case Right(a) => k(a).run
  })
  final override def map[B](k: A => B): EitherT[M,E,B] = new EitherT[M,E,B](run map {
    case l : Left[_,_] => l.asInstanceOf[Either[E,B]]
    case Right(a) => Right(k(a))
  })
  final override def |[B>:A](n: => EitherT[M,E,B]): EitherT[M,E,B] = new EitherT[M,E,B](run flatMap { 
    case _ : Left[_,_] => n.run
    case r => M.pure(r)
  })
  final override def handling[B>:A](k: E => EitherT[M,E,B]): EitherT[M,E,B] = new EitherT[M,E,B](run flatMap {
    case Left(e) => k(e).run
    case r => M.pure(r)
  })
}
        
object EitherT {
  class monadOr[M[+_],E](val M:Monad[M])
  extends MonadOr[({type λ[+X] = EitherT[M,E,X]})#λ]
     with Monad.Transformer[({type λ[N[+_],+B] = EitherT[N,E,B]})#λ]
     with Raise[({type λ[+X] = EitherT[M,E,X]})#λ,E] {
    import M._
    def pure[A](a: A): EitherT[M,E,A] = new EitherT[M,E,A](M.pure(Right(a)))
    def bind[A,B](m: EitherT[M,E,A])(k: A => EitherT[M,E,B]): EitherT[M,E,B] = m flatMap k
    override def apply[A,B](m: EitherT[M,E,A])(k: A => B): EitherT[M,E,B] = m map k
    override def or[A](x: EitherT[M,E,A], y: => EitherT[M,E,A]): EitherT[M,E,A] = x | y
    def handle[A](m: EitherT[M,E,A])(k: E => EitherT[M,E,A]): EitherT[M,E,A] = m handling k 
    def lift[N[+_],A](n: N[A])(implicit N:Monad[N]): EitherT[N,E,A] = new EitherT[N,E,A](N(n)(Right[E,A]))
    def raise(a: E): EitherT[M,E,Nothing] = new EitherT[M,E,Nothing](M.pure(Left(a)))
  }
  def monad[M[+_],E](implicit M:Monad[M]): EitherT.monadOr[M,E] = new EitherT.monadOr[M,E](M)
}
