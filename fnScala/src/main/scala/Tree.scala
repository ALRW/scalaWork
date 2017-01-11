sealed trait Tree[+A]


case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(fn: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => fn(fold(l)(f)(fn), fold(r)(f)(fn))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 1)(1 + _ + _)


  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)


  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 0)((x,y) => 1 + (x max y))


  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(z => Leaf(f(z)): Tree[B])((x,y) => Branch(x, y))

}
