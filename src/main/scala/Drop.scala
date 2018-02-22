object Drop {

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) return l;
    l match {
      case Nil => Nil;
      case (x :: xs) => drop(xs, n-1)
    }
  }
}
