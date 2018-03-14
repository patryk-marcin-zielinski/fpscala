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
    this.flatMap(a => ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if(f(a)) Some(a) else None)
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  (a, b) match {
    case (Some(ac), Some(bc)) => Some(f(ac, bc))
    case _ => None
  }
}