object ListToList {

  def main(args: Array[String]): Unit = {
    //should print same list
    println(FoldImplementation.foldRight(List(1,2,3), Nil:List[Int])(_ :: _))
  }
}
