object FoldImplementation {


  def foldRight[A,B](as: List[A], z: B)(f: (A, => B) => B): B = as match {
    case Nil => z
    case (x :: xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case (x :: xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

}
