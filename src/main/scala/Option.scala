sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case _ => None
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse[Option[B]](ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if(f(a)) Some(a) else None)
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] = {
    ao.flatMap(a => bo.map(b => f(a, b)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List[A]()))((ao, acc) => acc.flatMap(accs => ao.map(a => a :: accs)))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List[B]()))((a, acc) => acc.flatMap(ac => f(a).map(b => b :: ac)))
  }

}