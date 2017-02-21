import scala.annotation.tailrec

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
