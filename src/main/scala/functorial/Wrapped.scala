package functorial

import scala.collection.generic

trait Wrapped[+A] {
  def value: A
}

object Wrapped { 
  implicit def unwrap[A](wrapped: Wrapped[A]): A = wrapped.value
}

