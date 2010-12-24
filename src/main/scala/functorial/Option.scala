package functorial

object Option extends MonadPlus[Option] { module => 
  def pure[A](a: A): Option[A] = Some(a)
  def bind[A,B](a: Option[A])(f: A => Option[B]) = a flatMap f
  def empty = None
  def or[A](x: Option[A], y: => Option[A]): Option[A] = x orElse y
}
