object DropWhile {

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = {
    xs match {
      case (y :: ys) if f(y) => dropWhile(ys, f)
      case _ => xs
    }
  }


}
