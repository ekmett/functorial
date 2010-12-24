package functorial

import scala.collection.generic
import functorial.Extensions._

trait Applicative[F[+_]] extends Functor[F] { module => 
  // implicit def self: Applicative[F] = this
  def ap[A,B](f: F[A => B], a: F[A]): F[B]
  def pure[A](a: A): F[A]

  def lift1[A,B](a: F[A])(f: A => B): F[B] =
    f !! a
  def lift2[A,B,C](a: F[A],b: F[B])(f: (A,B) => C): F[C] =
    f !! a !* b 
  def lift3[A,B,C,D](a: F[A],b: F[B],c: F[C])(f: (A,B,C) => D): F[D] =
    f !! a !* b !* c
  def lift4[A,B,C,D,E](a: F[A],b: F[B],c: F[C],d: F[D])(f: (A,B,C,D) => E): F[E] = 
    f !! a !* b !* c !* d

  def unit: F[Unit] = pure[Unit](())

  override implicit def syntax[A](m: F[A]): Applicative.Syntax[F,A] = new Applicative.Syntax[F,A] {
    val companion = module
    def value = m
  }

  implicit def staticFunction1[A,B](m: F[A => B]): Applicative.StaticFunction1[F,A,B] 
                                             = new Applicative.StaticFunction1[F,A,B] {
    val companion = module
    def value = m
  }
  implicit def staticFunction2[A,B,C](m: F[(A,B) => C]): Applicative.StaticFunction2[F,A,B,C] 
                                                   = new Applicative.StaticFunction2[F,A,B,C] {
    val companion = module
    def value = m
  }
  implicit def staticFunction3[A,B,C,D](m: F[(A,B,C) => D]): Applicative.StaticFunction3[F,A,B,C,D] 
                                                       = new Applicative.StaticFunction3[F,A,B,C,D] {
    val companion = module
    def value = m
  }
}

object Applicative { 
  trait Syntax[F[+_],+A] extends Functor.Syntax[F,A] 
                            with HasCompanion[Applicative[F]] { m => 
    def <**>[B](n: F[A => B]): F[B] = companion.lift2(m,n)((a,f) => f(a)) 
    def ~>[B](n: F[B]): F[B] = companion.lift2(m,n)((_,b) => b)
    def <~[B](n: F[B]): F[A] = companion.lift2(m,n)((a,_) => a)
    def ~[B](n: F[B]): F[(A,B)] = companion.lift2(m,n)((a,b) => (a,b))
    def forever: F[Nothing] = {
      lazy val loop : F[Nothing] = ~>(loop)
      loop
    }
    // def replicate(n: Int): F[List[A]]
  }
  trait StaticFunction1[F[+_],-A,+B] extends Syntax[F,A => B] { m =>
    def !!*(a: F[A]): F[B] = companion.ap(m, a)
    def !*(a: F[A]): F[B] = companion.ap(m, a)
  }
  trait StaticFunction2[F[+_],-A,-B,+C] extends Syntax[F,(A,B) => C] { m =>
    def !!*(a: F[A],b: F[B]): F[C] = {
      val dict = companion; import dict._
      apply[(A,B)=>C,A=>B=>C](_.curried, m) !* a !* b
    }
    def !*(a: F[A]): F[B => C] = {
      val dict = companion; import dict._
      apply[(A,B)=>C,A=>B=>C](_.curried, m) !* a
    }
  }
  trait StaticFunction3[F[+_],-A,-B,-C,+D] extends Syntax[F,(A,B,C) => D] { m =>
    def !!*(a: F[A],b: F[B], c: F[C]): F[D] = {
      val dict = companion; import dict._
      apply[(A,B,C)=>D,A=>B=>C=>D](f => f.curried, m) !* a !* b !* c
    }
    def !*(a: F[A]): F[(B,C) => D] = {
      val dict = companion; import dict._
      apply[(A,B,C)=>D,A=>(B,C)=>D](f => a => (b,c) => f(a,b,c), m) !* a
    }
  }
}

