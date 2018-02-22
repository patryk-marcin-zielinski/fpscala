object Init {

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case (_ :: Nil) => Nil
      case (x :: xs) => x :: init(xs)
    }
  }

}
