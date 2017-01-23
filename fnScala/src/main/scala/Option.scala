
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

  def getOrElse[B >: A](default: => B): B = this match{
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]):Option[B] = this match {
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
      if(ys.isEmpty) None else Some(ys.sum / ys.length)
    }
    mean(xs) flatMap(m => mean(xs map (y => math.pow(y - m, 2))))
  }

}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]
