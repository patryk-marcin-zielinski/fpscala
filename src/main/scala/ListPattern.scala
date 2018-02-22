object ListPattern {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case (x :: xs) => x + sum(xs)
  }

  def print() : Unit  = {
    val x = List(1,2,3,4,5) match {
      case (x :: 2 :: 4 :: _) => x
      case Nil => 42
      case (x :: y :: 3 :: 4 :: _) => x + y
      case (h :: t) => h + sum(t)
      case _ => 101
    }
    println(x);
  }

  def main(args: Array[String]): Unit = {
    print();
  }

}
