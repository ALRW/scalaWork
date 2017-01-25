sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) if f(x) == None => None
    case Some(y) => f(y)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => this
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }


}

object Option {

  def variance(xs: Seq[Double]): Option[Double] = {

    def mean(ys: Seq[Double]): Option[Double] = {
      if (ys.isEmpty) None else Some(ys.sum / ys.length)
    }

    mean(xs) flatMap (m => mean(xs map (y => math.pow(y - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case Cons(h, t) => h.flatMap(hi => sequence(t).map(li => Cons(hi, li)))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]):Option[List[B]] = {
    Option.sequence(List.map(a)(f))
  }


}

case class Some[A](get: A) extends Option[A]

case object None extends Option[Nothing]
