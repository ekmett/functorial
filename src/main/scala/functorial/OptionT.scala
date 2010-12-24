package functorial

sealed class OptionT[M[+_],+A](val run: M[Option[A]])(implicit val M:Monad[M]) 
     extends MonadPlus.Syntax[({type λ[+X] = OptionT[M,X]})#λ,A]
        with Monad.Transformer.Syntax[({type λ[N[+_],+B] = OptionT[N,B]})#λ,M,A]
        with HasCompanion[OptionT.monadPlus[M]] {
  import M._
  final def value = this
  final val F : OptionT.monadPlus[M] = new OptionT.monadPlus[M](M)
  final override def flatMap[B](k: A => OptionT[M,B]): OptionT[M,B] = new OptionT[M,B](run flatMap { 
    case None    => M.pure(None)
    case Some(a) => k(a).run
  })
  final override def map[B](k: A => B): OptionT[M,B] = new OptionT[M,B](run map {
    case None    => None
    case Some(a) => Some(k(a))
  })
  final override def |[B>:A](n: => OptionT[M,B]): OptionT[M,B] = new OptionT[M,B](run flatMap { 
    case None => n.run
    case s => M.pure(s)
  })
}
        
object OptionT {
  class monadPlus[M[+_]](val M:Monad[M])
  extends MonadPlus[({type λ[+X] = OptionT[M,X]})#λ]
     with Monad.Transformer[({type λ[N[+_],+B] = OptionT[N,B]})#λ] {
    import M._
    def pure[A](a: A): OptionT[M,A] = new OptionT[M,A](M.pure(Some(a)))
    def bind[A,B](m: OptionT[M,A])(k: A => OptionT[M,B]): OptionT[M,B] = m flatMap k
    override def apply[A,B](m: OptionT[M,A])(k: A => B): OptionT[M,B] = m map k
    override def or[A](x: OptionT[M,A], y: => OptionT[M,A]): OptionT[M,A] = x | y
    def empty: OptionT[M,Nothing] = new OptionT[M,Nothing](M.pure(None))
    def lift[N[+_],A](n: N[A])(implicit N:Monad[N]): OptionT[N,A] = new OptionT[N,A](N(n)(Some[A]))
  }
  def monad[M[+_],E](implicit M:Monad[M]): OptionT.monadPlus[M] = new OptionT.monadPlus[M](M)
}
