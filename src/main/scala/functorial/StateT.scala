package functorial

sealed class StateT[M[+_]:Monad,S,+A](f: S => M[(A,S)]) extends 
    Monad.Syntax[({type λ[+X] = StateT[M,S,X]})#λ, A] with 
    Monad.Transformer.Syntax[({type λ[N[+_],+B] = StateT[N,S,B]})#λ,M,A] with 
    HasCompanion[StateT.monad[M,S]]{ m => 
  val underlyingMonad: Monad[M] = implicitly[Monad[M]]
  def value = this
  final def apply(s: S): M[(A,S)] = f(s)
  final val companion = StateT.monad[M,S]
  final override def flatMap[B](k: A => StateT[M,S,B]): StateT[M,S,B] = new StateT[M,S,B](s => {
    import underlyingMonad._
    f(s) flatMap (as => k(as._1)(as._2))
  })
}

object StateT { 
  def apply[M[+_]:Monad,S,A](f: S => M[(A,S)]) = new StateT[M,S,A](f)
  def monad[M[+_]:Monad,S]: monad[M,S] = new monad[M,S]
  class monad[M[+_]:Monad,S] extends Monad[({type λ[+X] = StateT[M,S,X]})#λ] with FunctorState[({type λ[+X] = StateT[M,S,X]})#λ, S] with Monad.Transformer[({type λ[N[+_],+B] = StateT[N,S,B]})#λ] {
    val base: Monad[M] = implicitly[Monad[M]]
    def pure[A](a: A) = StateT[M,S,A](s => base.pure((a, s)))
    def bind[A,B](f: A => StateT[M,S,B], m: StateT[M,S,A]) = m flatMap f 
    override def get: StateT[M,S,S] = StateT[M,S,S](s => base.pure((s,s)))
    def state[A](f: S => (A,S)) = StateT[M,S,A](s => base.pure(f(s)))
    def lift[N[+_]:Monad,A](m: N[A]): StateT[N,S,A] = StateT[N,S,A](s => 
      implicitly[Monad[N]].apply[A,(A,S)](a => (a,s), m)
    )
  }
}
