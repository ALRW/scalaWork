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

  def unfoldMap[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Const(head,tail) => Some((f(head()), tail()))
      case _ => None
    }

  def unfoldTake(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Const(h, t), 1) => Some(h(), (Empty, 0))
      case (Const(h, t), n) if n > 1=> Some(h(), (t(), n - 1))
      case _ => None
    }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Const(h,t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this,s)){
      case (Const(h1,t1), Const(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s)){
      case (Const(h1,t1), Const(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Const(h,t)) => Some((None, Some(h())), (Empty, t()))
      case (Const(h,t), Empty) => Some((Some(h()), None), (t(), Empty))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{
      case (h1,h2) => h1 == h2
    }
  }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case Const(h,t) => Some(Const(h, t), t())
      case _ => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](i: B)(f: (A, => B) => B): Stream[B] =
    foldRight((i, Stream(i)))((a, b) => (f(a, b._1), Stream.const(f(a, b._1), b._2)))._2
}

case object Empty extends Stream[Nothing]

case class Const[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def const[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Const(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = const(a, constant(a))

  def from(n: Int): Stream[Int] = const(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibber(p: Int, c: Int): Stream[Int] = {
      const(c, fibber(c, p + c))
    }
    const(0, fibber(0,1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => const(h, unfold(s)(f))
    case None => empty
  }


  def unfoldOnes: Stream[Int] =
    unfold(1)(_ => Some(1,1))

  def unfoldConstant[A](c: A):Stream[A] =
    unfold(c)(_ => Some(c,c))

  def unfoldFrom(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def unfoldfibs: Stream[Int] =
    unfold((0,1))(x => Some((x._1,(x._2, x._1 + x._2))))

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else const(as.head, apply(as.tail: _*))
  }
}
