package functorial

trait Pointed[F[+_]] extends Functor[F] with Pure[F] { module => 
  override implicit def syntax[A](m: F[A]): Pointed.Syntax[F,A] 
                                      = new Pointed.Syntax[F,A] {
    val F = module
    def value = m
  }
}

object Pointed { 
  trait Syntax[F[+_],+A] extends Functor.Syntax[F,A] 
                            with Pure.Syntax[F,A]
                            with HasCompanion[Pointed[F]]
}

