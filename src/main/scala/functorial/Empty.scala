package functorial

import scala.collection.generic

trait Empty[F[+_]] extends Companion { module => 
  def empty: F[Nothing]
}
object Empty { 
  trait Syntax[F[+_],+A] extends HasCompanion[Empty[F]] 
                           with Wrapped[F[A]]
}

