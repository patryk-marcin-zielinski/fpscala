object Join {

  def join[A](list: List[List[A]]) : List[A] = {
      FoldImplementation.foldLeft(list,List[A]())((b,a) => Append.append(b,a))
  }

}
