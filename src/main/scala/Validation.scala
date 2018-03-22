sealed trait Validation[+E, +A] {
  def map[B](f : A => B) = {
    this match {
      case Value(a) => Value(f(a))
      case e @ Error(_) => e
    }
  }

  def orElse[EE >: E,B >: A](b: => Validation[EE, B]): Validation[EE, B] = {
    this match {
      case Error(_) => b
      case v @ Value(_) => v
    }
  }

  def map2[EE >: E,B,C](a: Validation[EE, B])(f: (A, B) => C) = {
    (this, a) match {
      case (Value(a), Value(b)) => Value(f(a,b))
      case (v @ Value(_), Error(_)) => v
      case (Error(_), v @ Value(_)) => v
      case (Error(aerr), Error(berr)) => Error(aerr ++ berr)
    }
  }
}

case class Error[+E](get: Seq[E]) extends Validation[E, Nothing]
case class Value[+A](get: A) extends Validation[Nothing, A]

object Validation {
  def traverse[E, A, B](as: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] = {
    ???
    //as.foldRight[Validation[E, List[B]]](Value(Nil))((av, accv) => f(av).map2(accv)((a, acc) => a :: acc))
  }
}




