
object FoldLeftToRight {

  def foldLeft[A,B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case (y :: ys) => f(foldLeft(ys, acc)(f), y)
  }

  def foldRight[A,B](as: List[A], acc: B)(f: (A, => B) => B): B = as match {
    case Nil => acc
    case (x :: xs) => foldRight(xs, f(x, acc))(f)
  }

//  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
//    case Nil => z
//    case (x :: xs) => foldLeft(xs, f(z, x))(f)
//  }

  //foldRightViaFoldLeft_1(List(1,2,3), 0, (a, b) = a+b)
  //foldLeft(List(1,2,3), (b) => b) ((g,a) => b => g(a+b))(0)
  //foldLeft(List(2,3), (b => b, 1) =>b => g(a+b))((g,a) => b => g(a+b))
  //foldLeft(List(2,3), b => 1+b)((g,a) => b => g(a+b))
  //foldLeft(List(3), (b => 1+b, 2) => b => g(2+b)((g,a) => b => g(a+b))
  //foldLeft(List(3), b => 1+(2+b)((g,a) => b => g(a+b))
  //foldLeft(Nil, (b => 1+(2+b),3) => b => g(3+b))((g,a) => b => g(a+b))
  //b => (1+2+(3+b))) (0) => (1+2+3+0) = 7


  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

}
