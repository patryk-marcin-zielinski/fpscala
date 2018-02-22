object SetHead {

  def replaceHead[A](head: A, xs: List[A]): List[A] =
    xs match {
      case (x :: xs) => head :: xs
      case Nil => Nil;
    }

}
