object FilterByFlatMap {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    FlatMap.flatMap1(as)(a => if(f(a)) List(a) else List())
  }

}
