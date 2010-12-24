package functorial

sealed class State[S,+A](f: S => (A,S)) extends Monad.Syntax[({type λ[+X] = State[S,X]})#λ, A] { m => 
  final def apply(s: S): (A,S) = f(s)
  final override def flatMap[B](f: A => State[S,B]) = State[S,B](s => {
    val as = m(s)
    f(as._1)(as._2)
  })
  val companion = State.monad[S]
  def value = this
}

object State {
  def apply[S,A](f: S => (A,S)) = new State(f)
  def monad[S]: Monad[({type λ[+X] = State[S,X]})#λ] 
           with FunctorState[({type λ[+X] = State[S,X]})#λ, S] = 
            new Monad[({type λ[+X] = State[S,X]})#λ] 
           with FunctorState[({type λ[+X] = State[S,X]})#λ, S] {
    def pure[A](a: A): State[S,A] = State[S,A](s => (a, s))
    def bind[A,B](f: A => State[S,B], m: State[S,A]): State[S,B] = m flatMap f 
    override def get: State[S,S] = State[S,S](s => (s,s))
    def state[A](f: S => (A,S)): State[S,A] = State[S,A](f)
  }
}

