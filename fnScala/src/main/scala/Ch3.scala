import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](x: A, xs: List[A]): List[A] = xs match{
    case Nil => List(x)
    case Cons(y, ys) => Cons(x, ys)
  }

  @tailrec
  def drop[A](ls: List[A], n: Int): List[A] = {
    if (n <= 0) ls
    else drop(tail(ls), n -1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case ls @ Cons(x, _) if !f(x) => ls
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
  }

  def append[A](a1: List[A], a2: List[A]):List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]):List[A] = ???
}


object Ch3 {
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
}
