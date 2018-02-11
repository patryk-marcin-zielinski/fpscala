object Uncurry {

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b);
  }

  def main(args: Array[String]): Unit = {
    println(uncurry((a:Int) => (b: Int) => a+b)(2,4))
  }

}
