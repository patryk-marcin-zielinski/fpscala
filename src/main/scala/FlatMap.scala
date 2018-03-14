object FlatMap {
  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = {
      Join.join(Transform.map(as)(f))
  }

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
    FoldImplementation.foldRight(as, List[B]())((a, b) => Append.append(f(a), b))
  }
}
