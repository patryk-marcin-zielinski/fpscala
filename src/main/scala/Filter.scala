object Filter {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    FoldImplementation.foldRight(as, List[A]())((a, b) => if(f(a)) a :: b else b)
  }
}
