object ReverseList {

  def reverse1[A](xs: List[A]) : List[A] = {
    FoldImplementation.foldLeft(xs, Nil: List[A])((acc, x) => x :: acc)
  }

  def reverse2[A](xs: List[A]) : List[A] = {
    FoldImplementation.foldRight(xs, Nil: List[A])((x, acc) => acc ++ List(x))
  }
}
