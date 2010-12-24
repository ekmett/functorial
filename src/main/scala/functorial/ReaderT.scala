package functorial

class ReaderT[M[+_],E,+A](f: E => M[A])(implicit val base: Monad[M]) 
     extends Monad.Syntax[({type λ[+X] = ReaderT[M,E,X]})#λ, A] 
        with Monad.Transformer.Syntax[({type λ[N[+_],+B] = ReaderT[N,E,B]})#λ,M,A] 
        with HasCompanion[ReaderT.monad[M,E]]{ m => 
  def value = this
  final def apply(e: E): M[A] = f(e)
  final val companion = ReaderT.monad[M,E]
  final override def flatMap[B](k: A => ReaderT[M,E,B]) = new ReaderT[M,E,B](e => 
    base.bind(f(e))(a => k(a)(e))
  )
}

object ReaderT { 
  def apply[M[+_]:Monad,E,A](f: E => M[A]) = new ReaderT[M,E,A](f)
  def monad[M[+_]:Monad,E]: monad[M,E] = new monad[M,E]
  class monad[M[+_]:Monad,E] extends Monad[({type λ[+X] = ReaderT[M,E,X]})#λ] 
                                with PointedReader[({type λ[+X] = ReaderT[M,E,X]})#λ, E] 
                                with Monad.Transformer[({type λ[N[+_],+B] = ReaderT[N,E,B]})#λ] {
    val base: Monad[M] = implicitly[Monad[M]]
    override def pure[A](a: A) = ReaderT[M,E,A](_ => base.pure(a))
    def bind[A,B](m: ReaderT[M,E,A])(f: A => ReaderT[M,E,B]) = m flatMap f 
    override def ask: ReaderT[M,E,E] = ReaderT[M,E,E](base.pure)
    def reader[A](f: E => A) = ReaderT[M,E,A](e => base.pure(f(e)))
    def lift[N[+_]:Monad,A](m: N[A]): ReaderT[N,E,A] = ReaderT[N,E,A](_ => m)
  }
}
