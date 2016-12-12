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

  def init[A](l: List[A]):List[A] ={

    def helper[A](ls: List[A], xs: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(x, Nil) => xs
      case Cons(x, xy) => Cons(x, helper(xy, xs))
    }

    helper(l, List())
  }

  def foldright[A, B](as: List[A], z: B)(f: (A, B) => B):B = as match{
    case Nil => z
    case Cons(x, xs) => f(x, foldright(xs, z)(f))
  }

  def length[A](l: List[A]): Int = {
    foldright(l, 0)((a,b) => b + 1)
  }

  @tailrec
  def foldLeft[A,B](as:List[A], z: B)(f: (B,A) => B):B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldSum(l: List[Int]): Int = {
    foldLeft(l,0)((x,y) => x + y)
  }

  def foldProduct(l: List[Int]): Int = {
    foldLeft(l, 1)((x,y) => x * y)
  }

  def foldLength[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,y) => x + 1)
  }

  def foldAppend[A](l: List[A], k: List[A]): List[A] = {
    foldright(l,k)((x, y) => Cons(x,y))
  }

  def addOne(l: List[Int]): List[Int] = l match{
    case Nil => Nil
    case Cons(h,t) => Cons(h + 1, addOne(t))
  }

  def doubleToString(l:List[Double]): List[String] = l match{
    case Nil => Nil
    case Cons(h,t) => Cons(h.toString, doubleToString(t))
  }

  def map[A, B](l:List[A])(f: A => B): List[B] = l match{
    case Nil => Nil
    case Cons(h,t) => Cons(f(h), map(t)(f))
  }

  def filter[A](l:List[A])(f: A=> Boolean): List[A] = l match{
    case Nil => Nil
    case Cons(h,t) if f(h) => Cons(h, filter(t) (f))
    case Cons(h,t) if !f(h) => filter(t) (f)
  }

}



object Ch3 {
  val x: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
}
