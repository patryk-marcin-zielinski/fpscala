object VeryLargeList {


  //if(1>10) => 1 + foldRight(xs, 0)(f)
  //if(2>10) => 1 + (2 + foldRight(xs, 0)(f))
  //if(10>10) => (1 + (2 + (3 +(4 +(5 + (6+ (7+ (8 +(9 +(1))))))))))

  def main(args: Array[String]): Unit = {
    val xs = (1 to 1000000).toList // List in scala are evaluated now, there is no lazy structure
    val catamorphism: (Int, => Int) => Int = (a, b) => if (a > 10) 1 else a + b;
    println("Start folding");
    println(FoldImplementation.foldRight(xs, 0)(catamorphism));
  }

}
