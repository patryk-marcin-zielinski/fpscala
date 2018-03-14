object Transform {
  def map[A,B](list:List[A])(f: A => B) : List[B] = {
    FoldImplementation.foldRight(list, List[B]())((a, b) => f(a) :: b)
  }
}
