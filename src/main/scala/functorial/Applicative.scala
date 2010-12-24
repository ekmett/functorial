package functorial

import functorial.Extensions._

trait Applicative[F[+_]] extends Pointed[F] { module => 
  def apply[A,B](a: F[A])(f: A => B): F[B] = ap(a)(pure(f))
  def ap[A,B](f: F[A])(a: F[A => B]): F[B]
  def lift1[A,B](a: F[A])(f: A => B): F[B] = a map f
  def lift2[A,B,C](a: F[A],b: F[B])(f: (A,B) => C): F[C] = f !! a !* b 
  def lift3[A,B,C,D](a: F[A],b: F[B],c: F[C])(f: (A,B,C) => D): F[D] = f !! a !* b !* c
  def lift4[A,B,C,D,E](a: F[A],b: F[B],c: F[C],d: F[D])(f: (A,B,C,D) => E): F[E] = f !! a !* b !* c !* d
  override implicit def syntax[A](m: F[A]): Applicative.Syntax[F,A] = new Applicative.Syntax[F,A] {
    val F = module
    def value = m
  }
  implicit def static1[A,B](m: F[A => B]) = new Applicative.Static1[F,A,B](module,m)
  implicit def static2[A,B,C](m: F[(A,B) => C]) = new Applicative.Static2[F,A,B,C](module,m)
  implicit def static3[A,B,C,D](m: F[(A,B,C) => D]) = new Applicative.Static3[F,A,B,C,D](module,m)
}

object Applicative { 
  trait Syntax[F[+_],+A] extends Pointed.Syntax[F,A] 
                            with HasCompanion[Applicative[F]] { m => 
    def <**>[B](n: F[A => B]): F[B] = F.ap(m)(n)
    def ~>[B](n: F[B]): F[B] = F.lift2(m,n)((_,b) => b)
    def <~[B](n: F[B]): F[A] = F.lift2(m,n)((a,_) => a)
    def ~[B](n: F[B]): F[(A,B)] = F.lift2(m,n)((a,b) => (a,b))
    def forever: F[Nothing] = {
      lazy val loop : F[Nothing] = ~>(loop)
      loop
    }
    // def replicate(n: Int): F[List[A]]
  }
  sealed class Static1[F[+_],-A,+B](val F:Applicative[F], val value: F[A => B]) 
       extends Syntax[F,A => B] {
    final def !!*(a: F[A]): F[B] = F.ap(a)(value)
    final def !*(a: F[A]): F[B] = F.ap(a)(value)
  }
  sealed class Static2[F[+_],-A,-B,+C](val F:Applicative[F], val value: F[(A,B) => C]) 
       extends Syntax[F,(A,B) => C] {
    import F._
    final def !!*(a: F[A],b: F[B]): F[C] = F(value)(_.curried) !* a !* b
    final def !*(a: F[A]): F[B => C] = F(value)(_.curried) !* a
  }
  sealed class Static3[F[+_],-A,-B,-C,+D](val F:Applicative[F], val value: F[(A,B,C) => D]) 
       extends Syntax[F,(A,B,C) => D] {
    import F._
    final def !!*(a: F[A],b: F[B], c: F[C]): F[D] = F(value)(f => f.curried) !* a !* b !* c
    final def !*(a: F[A]): F[(B,C) => D] = F(value)(f => (a:A) => (b:B,c:C) => f(a,b,c)) !* a
  }
}
