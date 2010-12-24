package functorial

import scala.collection.generic

/*
// attempt to implement first order concepts by lowering higher order ones

trait Semigroup[A] extends Or[({trait λ[+X] = A})#λ,A] {
  override implicit def syntax[A](m: A): Semigroup.Syntax[A] 
                                   = new Semigroup.Syntax[A] {
    def companion = module
    def value = m
  }
}

object Semigroup {
  def apply[A](f: (A,A) => A) = new Semigroup[A] {
    def or[B](a: A, b: A): A = f(a,b)
  }
  trait Syntax[A] extends Or.Syntax[({trait λ[+X] = A)#λ,A] with HasCompanion[Semigroup[A]] { m => 
    // type argument required, even though not used due to lowering
    def |[B](n: A): A = companion.or(m,n)
  }
}

trait Unital[+A] extends Empty[({trait λ[+X] = A})#λ,A] {
  override implicit def syntax[A](m: A): Unital.Syntax[A]
                                   = new Unital.Syntax[A] {
    def companion = module
    def value = m
  }
}

object Unital { 
  def apply[A](zero: => A) = new Unital[A] {
    def empty = zero
  }
  trait Syntax[A] extends Empty.Syntax[({trait λ[+X] = A)#λ,A] with HasCompanion[Unital[A]]
}

trait Monoid[A] extends Semigroup[A] with Unital[A] {
  override implicit def syntax[A](m: A): Monoid.Syntax[A]
                                   = new Monoid.Syntax[A] {
    def companion = module
    def value = m
  }
}

object Monoid {
  def apply[A](e: A)(f: (A,A) => A) = new Monoid[A] {
    def or[B](a: A, b: A): A = f(a,b)
    def empty: A = e
  }
  trait Syntax[A] extends Semigroup.Syntax[A] 
                     with Unital.Syntax[A] 
                     with HasCompanion[Monoid[A]] {
  }
}
*/
