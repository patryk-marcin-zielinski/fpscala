object Length {

  def length[A](as: List[A]): Int = {
    FoldImplementation.foldRight(as, 0)((a,b) => b + 1)
  }

}
