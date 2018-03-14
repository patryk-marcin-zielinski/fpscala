object Append {
  def append[A](a1: List[A], a2: List[A]) : List[A] ={
    FoldImplementation.foldRight(a1, a2)((a, b) => a :: b)
  }
}
