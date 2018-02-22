
object FoldLeftToRight {

  def foldLeft[A,B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case (y :: ys) => f(foldLeft(ys, acc)(f), y)
  }

  def foldRight[A,B](as: List[A], acc: B)(f: (A, => B) => B): B = as match {
    case Nil => acc
    case (x :: xs) => foldRight(xs, f(x, acc))(f)
  }

}
