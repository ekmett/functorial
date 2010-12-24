package functorial

object List extends MonadPlus[List] { module => 
  def pure[A](a: A): List[A] = a :: Nil
  def bind[A,B](a: List[A])(f: A => List[B]) = a flatMap f
  def empty = Nil
  def or[A](xs: List[A], ys: => List[A]): List[A] = xs ::: ys
}

