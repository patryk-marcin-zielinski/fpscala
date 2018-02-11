object Compose {

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(compose((a:Int)=> "test", (b: String) => 1)("1"))
  }

}
