import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit())((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit())((a, _) => f(a))

  def anotherSortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](pa: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = pa.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = l.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(ps))(_.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 1 else 0))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
    choices(n(es).get)(es)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es =>
    choices(key(es).get)(es)

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es =>
    choices(pa(es).get)(es)

  implicit class ParOps[A](p: Par[A])

}

