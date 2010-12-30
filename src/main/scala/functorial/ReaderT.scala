package functorial

class ReaderT[M[+_],E,+A](f: E => M[A])(implicit val M:Monad[M]) 
     extends Monad.Syntax[({type λ[+X] = ReaderT[M,E,X]})#λ, A] 
        with Monad.Transformer.Syntax[({type λ[N[+_],+B] = ReaderT[N,E,B]})#λ,M,A] 
        with HasCompanion[ReaderT.monad[M,E]]{ m => 
  final def self = this
  final def apply(e: E): M[A] = f(e)
  final val F = ReaderT.monad[M,E]
  final override def flatMap[B](k: A => ReaderT[M,E,B]) = new ReaderT[M,E,B](e => 
    M.bind(f(e))(a => k(a)(e))
  )
}

object ReaderT { 
  def apply[M[+_]:Monad,E,A](f: E => M[A]) = new ReaderT[M,E,A](f)
  def monad[M[+_]:Monad,E]: monad[M,E] = new monad[M,E]
  sealed class monad[M[+_],E](implicit val M:Monad[M]) 
       extends Monad[({type λ[+X] = ReaderT[M,E,X]})#λ] 
          with PointedReader[({type λ[+X] = ReaderT[M,E,X]})#λ, E] 
          with Monad.Transformer[({type λ[N[+_],+B] = ReaderT[N,E,B]})#λ] {
    override def pure[A](a: A) = ReaderT[M,E,A](_ => M.pure(a))
    def bind[A,B](m: ReaderT[M,E,A])(f: A => ReaderT[M,E,B]) = m flatMap f 
    override def ask: ReaderT[M,E,E] = ReaderT[M,E,E](M.pure)
    def reader[A](f: E => A) = ReaderT[M,E,A](e => M.pure(f(e)))
    def lift[N[+_],A](m: N[A])(implicit N: Monad[N]): ReaderT[N,E,A] = ReaderT[N,E,A](_ => m)
  }
}
