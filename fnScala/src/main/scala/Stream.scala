sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Const(h,t) => Cons(h(), t().toList)
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Const(h,t) if n > 1 => Stream.const(h(), t().take(n-1))
    case Const(h, _) if n == 1 => Stream.const(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Const(h,t) if n > 1 => t().drop(n-1)
    case Const(h, t) if n == 1 => t()
    case _ => Stream.empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Const(h,t) if f(h()) => Stream.const(h(), t().takeWhile(f))
    case _ => Stream.empty
  }

  def exists(f: A => Boolean): Boolean = this match {
    case Const(h,t) => f(h()) || t().exists(f)
    case _ => false
  }

  def foldRight[B](x: => B)(f: (A, => B) => B): B = this match {
    case Const(h,t) => f(h(), t().foldRight(x)(f))
    case _ => x
  }

  def foldExists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean = this match {
    case Const(h, t) if t() == Stream.empty => f(h())
    case Const(h, t) => f(h()) && t().forAll(f)
  }

  def foldTakeWhile(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if(f(a)) Stream.const(a, b) else Stream.empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => Stream.const(f(a), b))

  def append[B >: A](x: => Stream[B]): Stream[B] =
    foldRight(x)((a,b) => Stream.const(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]

case class Const[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def const[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Const(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else const(as.head, apply(as.tail: _*))
  }

}
