object Zip {
  def zip[A,B, C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =  {
    (as, bs) match  {
      case (a :: ass, b :: bbs) => f(a, b) :: zip(ass, bbs)(f)
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }
}
