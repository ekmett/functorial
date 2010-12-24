package functorial

trait MonadRaise[F[+_],E] extends MonadOr[F]
                             with Raise[F,E] { module => 
  override implicit def syntax[A](m: F[A]): MonadRaise.Syntax[F,E,A]
                                      = new MonadRaise.Syntax[F,E,A] {
    val F = module
    def value = m
  }
}

object MonadRaise {
  trait Syntax[F[+_],E,+A] 
    extends MonadOr.Syntax[F,A]
       with Raise.Syntax[F,E,A]
       with HasCompanion[MonadRaise[F,E]]
}
