object Tail {

  def tail[A](xs: List[A]) : List[A] =
    xs match {
      case (_ :: xs) => xs
      case _ => Nil
    }

}
