package functorial

import scala.collection.generic

// a toy list example monad
class ListEmptyException extends Exception
sealed abstract class List[+A] extends MonadPlus.Syntax[List,A] { xs => 
  def head: A 
  def tail: List[A]
  def isEmpty: Boolean
  def uncons: Option[(A,List[A])]
  val companion: MonadPlus[List] = List
  def ::[B>:A](x: B): List[B] = Cons(x, xs)
  def value = this
}

object List extends MonadPlus[List] { 
  // def filter[A]
  def pure[A](a: A): List[A] = Cons(a, Nil)
  def bind[A,B](f: A => List[B], a: List[A]) = a flatMap f
  def empty = functorial.Nil
  def or[A](xs: List[A], ys: List[A]): List[A] = xs | ys
}

case object Nil extends List[Nothing] {
  def head: Nothing = throw new ListEmptyException
  def tail: List[Nothing] = throw new ListEmptyException
  def uncons: Option[(Nothing,List[Nothing])] = None
  def isEmpty: Boolean = true
  
  override def map[B](f: Nothing => B) = this
  override def flatMap[B](f: Nothing => List[B]) = this
  override def |[B](f: List[B]) = f
}

case class Cons[A](head: A, tail: List[A]) extends List[A] {
  def uncons: Option[(A,List[A])] = Some(head, tail)
  def isEmpty: Boolean = false

  override def map[B](f: A => B) = f(head) :: (tail map f)
  override def flatMap[B](f: A => List[B]) = f(head) | (tail flatMap f)
  override def |[B>:A](xs: List[B]) = head :: (tail | xs)
}
