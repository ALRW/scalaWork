sealed trait Either[+E, +A] {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case l: Left[E] => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case l: Left[E] => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r: Right[A] => r
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = (this, b) match {
    case (_, l: Left[E]) => l
    case (l: Left[E], _) => l
    case (Right(x), Right(y)) => Right(f(x,y))
  }
}

object Either {

  def sequence[E,A](l: List[Either[E,A]]): Either[E, List[A]] = l match {
    case Nil => Right(Nil)
    case Cons(h: Left[E], _) => h
    case Cons(h: Right[A], t) => h.flatMap(he => sequence(t).map(hi => Cons(he, hi)))
  }

  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    sequence(List.map(l)(f))
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

