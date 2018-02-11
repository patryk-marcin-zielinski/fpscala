object IsSorted {


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(index: Int) : Boolean = {
      if(index + 1  >= as.length) true;
      else if(ordered(as(index), as(index+1))) go(index + 1)
      else false;
    }

    if(as.length == 0) false
    else if(as.length == 1) true
    else go(0);
  }

  def main(args: Array[String]): Unit = {
      println(isSorted(Array("C", "D", "G"), (a: String, b: String) => a < b))
      println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
      println(isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b))
      println(isSorted(Array(1.0f, 2.0f, 100.0f), (a: Float, b: Float) => (a - b) < 0))
      println(isSorted(Array(10.0f, 2.0f, 1.0f), (a: Float, b: Float) => (a - b) < 0))
  }

}
