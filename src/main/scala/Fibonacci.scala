import scala.annotation.tailrec

object Fibonacci {

  // fib2(4)
  // (fib2(3) + fib2(2))
  // ((fib2(2) + fib2(1)) + (fib2(1) + fib2(0)))
  // (((fib2(1) + fib2(0)) + 1) + (1 + 0))
  // (((1 + 0) + 1) + (1 + 0))
  // ((1 + 1) + 1)
  // (2 + 1)
  // 3
  def fib2(nth: Int) : Int = {
    if(nth == 0) 0
    else if(nth == 1) 1
    else fib2(nth-1) + fib2(nth-2)
  }

  def fib1(nth: Int) : Int = {
    @tailrec
    def go(acc :List[Int], index: Int, end: Int): Int= {
      if(index == end) acc(0) + acc(1)
      else go((acc(0) + acc(1)) :: acc, index + 1, end)
    }
    if(nth == 0) 0
    else if(nth == 1) 1
    else go(List(1,0), 2, nth)
  }

  def main(args: Array[String]): Unit = {
    println(fib1(4))
    println(fib2(4))
  }
}
