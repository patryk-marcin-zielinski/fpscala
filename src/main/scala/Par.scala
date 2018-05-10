import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class TrackingMap[A,B,C](af: Future[A], bf: Future[B], f: (A , B) => C) extends Future[C] {

    def isDone = af.isDone && bf.isDone
    def get(timeout: Long, units: TimeUnit) = {
      val timeoutInMilis = units.convert(timeout, TimeUnit.MILLISECONDS)
      val startAt = System.currentTimeMillis()
      val a = af.get(timeoutInMilis, TimeUnit.MILLISECONDS)
      val take = System.currentTimeMillis() - startAt
      val b = bf.get(timeoutInMilis - take, TimeUnit.MICROSECONDS)
      f(a,b)
    }
    def isCancelled = af.isCancelled || bf.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = af.cancel(evenIfRunning) && bf.cancel(evenIfRunning)

    override def get() = {
      f(af.get, bf.get)
    }
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def mapWithTracking[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    TrackingMap(af, bf, f)
  }

  def asyncF2[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]()))((par, acc) => map2(par, acc)((v, list) => v :: list))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    unit(as.filter(f))
  }

  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF2(f))
    sequence(fbs)
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

}

