package functorial

class StateT[M[+_],S,+A](f: S => M[(A,S)])(implicit val M:Monad[M]) 
  extends Monad.Syntax[({type λ[+X] = StateT[M,S,X]})#λ, A] 
     with Monad.Transformer.Syntax[({type λ[N[+_],+B] = StateT[N,S,B]})#λ,M,A] 
     with HasCompanion[StateT.monad[M,S]]{ m => 
  final def self = this
  final def apply(s: S): M[(A,S)] = f(s)
  final val F = StateT.monad[M,S]
  final override def flatMap[B](k: A => StateT[M,S,B]) = new StateT[M,S,B](s => 
    M.bind(f(s))(as => k(as._1)(as._2)))
  final override def map[B](k: A => B) = new StateT[M,S,B](s => 
    M(f(s))({ case (a,s) => (k(a),s) }))
}

object StateT { 
  def apply[M[+_]:Monad,S,A](f: S => M[(A,S)]) = new StateT[M,S,A](f)
  def monad[M[+_]:Monad,S]: monad[M,S] = new monad[M,S]
  class monad[M[+_],S](implicit val M:Monad[M]) 
    extends Monad[({type λ[+X] = StateT[M,S,X]})#λ] 
       with PointedState[({type λ[+X] = StateT[M,S,X]})#λ, S] 
       with Monad.Transformer[({type λ[N[+_],+B] = StateT[N,S,B]})#λ] {
    def bind[A,B](m: StateT[M,S,A])(f: A => StateT[M,S,B]) = m flatMap f 
    def state[A](f: S => (A,S)) = StateT[M,S,A](s => M.pure(f(s)))
    def lift[N[+_],A](n: N[A])(implicit N: Monad[N]): StateT[N,S,A] = StateT[N,S,A](s => 
      N(n)(a => (a,s)))
  }
}
