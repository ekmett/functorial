package functorial

class State[S,+A](f: S => (A,S)) extends Monad.Syntax[({type λ[+X] = State[S,X]})#λ, A] { m => 
  final val F = new State.monad[S]
  final def value = this
  final def apply(s: S): (A,S) = f(s)
  final override def flatMap[B](g: A => State[S,B]) = new State[S,B](s => { val (a,s1) = f(s); g(a)(s1) })
  final override def map[B](g: A => B) = new State[S,B](s => { val (a,s1) = f(s); (g(a),s1) })
}

object State {
  def apply[S,A](f: S => (A,S)) = new State(f)
  class monad[S] extends Monad[({type λ[+X] = State[S,X]})#λ] 
                    with PointedState[({type λ[+X] = State[S,X]})#λ, S] {
    override def pure[A](a: A) = new State(s => (a,s))
    override def apply[A,B](m: State[S,A])(f: A => B) = new State[S,B](s => { val (a,s1) = m(s); (f(a),s1) })
    def bind[A,B](m: State[S,A])(f: A => State[S,B]) = new State[S,B](s => { val (a,s1) = m(s); f(a)(s1) })
    def state[A](f: S => (A,S)): State[S,A] = new State[S,A](f)
  }
}
