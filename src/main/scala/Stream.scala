
sealed trait Stream[+A] {

  def headOption: Option[A] =
    Stream.foldRight[A, Option[A]](this, None)((a, _) => Some(a) )


  def toList: List[A] = this match {
    case Cons(head, tail) => head() :: tail().toList
    case End => Nil
  }

  def getTail: Stream[A] = {
    this match {
      case Cons(_, tail) => tail();
      case End => End;
    }
  }

  def take(nth: Int): Stream[A] = {
    Stream.take(this, nth);
  }

  def drop(nth: Int): Stream[A] = {
    Stream.drop(this, nth);
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    Stream.takeWhile_1(this)(p)
  }

  def exists(p: A => Boolean): Boolean = Stream.foldRight(this, false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = Stream.foldRight(this, true)((a, b) => p(a) && b)

  def filter(p: A => Boolean): Stream[A] = Stream.foldRight[A, Stream[A]](this, End)((a,acc) => if(p(a)) Stream.cons(a, acc) else acc )

  def map[B](f: A => B): Stream[B] = Stream.foldRight[A, Stream[B]](this, End)((a,acc) => Stream.cons(f(a), acc))

  def append[E >: A](as: => Stream[E]) : Stream[E] = {
    Stream.foldRight(this, as)((a, acc) => Stream.cons(a, acc))
  }
  /**
   * Stream(1,2,3,4,5) f(a) = Stream(a+1)
   * Stream(2).append(foldRight(Stream(2,3,4,5), () => End)(f(a) = Stream(a+1)))
   * Cons(() => 2, () => foldRight(Stream(2,3,4,5), () => End)(f(a) = Stream(a+1)))
   */
  def flatMap[B](f: A => Stream[B]) : Stream[B] = {
    Stream.foldRight[A, Stream[B]](this, End)((a, acc) => f(a).append(acc))
  }

}

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

case object End extends Stream[Nothing]

object Stream {


  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val evalHead = head
    lazy val evalTail = tail
    Cons(() => evalHead, () => evalTail)
  }

  def end[A]: Stream[A] = End

  def foldRight[A, B](st: Stream[A], acc: => B)(f: (A, => B) => B): B = {
    st match {
      case Cons(head, tail) => f(head(), foldRight[A, B](tail(), acc)(f))
      case End => acc
    }
  }

  def drop[A](as: Stream[A], nth: Int): Stream[A] = {
    as match {
      case End => End
      case stream@Cons(_, _) if nth == 0 => stream;
      case Cons(_, tail) => drop(tail(), nth - 1)
    }
  }


  def take[A](as: Stream[A], nth: Int): Stream[A] = {
    as match {
      case Cons(head, tail) if nth > 0 => Cons(head, () => take[A](tail(), nth - 1))
      case _ => End
    }
  }

  def takeWhile_1[A](as: Stream[A])(p: A => Boolean): Stream[A] = {
    as match {
      case Cons(head, tail) if p(head()) => Stream.cons(head(), takeWhile_1(tail())(p))
      case _ => End
    }
  }

  def takeWhile_2[A](as: Stream[A])(p: A => Boolean): Stream[A] = {
    foldRight[A, Stream[A]](as, End)((a, acc) => if (p(a)) Stream.cons(a, acc) else End)
  }


  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) end else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = unfold(a)((a) => Some((a,a)))

  def from(n: Int): Stream[Int] = unfold(n)((n) => Some((n,n+1)))

  def fib(): Stream[Int] = unfold((0,1))((fib) => Some(fib._1,(fib._2, fib._1+fib._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z)
        .map(state =>  Stream.cons(state._1, unfold(state._2)(f)))
        .getOrElse(End)
  }

  def mapViaUnfold[A, B](as: Stream[A], f: A => B): Stream[B] = {
    unfold(as)(st => {
        st.headOption.map(head => (f(head), st.getTail))
    })
  }

  def takeViaUnfold[A](as: Stream[A], nth: Int): Stream[A] = {
    unfold((nth,as))(tuple =>
      if(tuple._1 > 0)
        tuple._2.headOption.map(head => (head,(tuple._1 - 1, as.getTail)))
      else
        None
    )
  }

  def takeWhileViaUnfold[A](as: Stream[A]) (p: A => Boolean) : Stream[A] = {
    unfold(as)(st => st.headOption.filter(p).map(head => (head, st.getTail)))
  }

  def zipWithViaUnfold[A, B,C](s1: Stream[A], s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((s1,s2))((all) => all._1.headOption.flatMap(a => all._2.headOption.map(b => (f(a,b),(all._1.getTail, all._2.getTail)))))
  }

  def zipAll[A,B](s1:Stream[A],s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((s1,s2))(all =>
        Some((all._1.headOption, all._2.headOption))
          .flatMap {
            case (None, None) => None
            case a @ (_, _) => Some(a)
          }
        .map(tuple => (tuple, (all._1.getTail, all._2.getTail)))
    )
  }

  def startsWith_1[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    zipWithViaUnfold(s1,s2)(_ == _).forAll((a) => a)
  }

  def startsWith_2[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    zipAll(s1,s2).takeWhile({
      case (Some(_), _) => true
      case _ => false
    })
    .map((tuple) => tuple._1.flatMap(a => tuple._2.map(b => a == b)).getOrElse(false))
    .forAll(a => a)
  }

  def tails[A] (st: Stream[A]): Stream[Stream[A]] = {
    unfold(st)(sub => sub.headOption.map(head => (sub, sub.getTail)))
  }

  def scanRight_1[A, B](st: Stream[A], acc: => B)(f: (A, => B) => B) : Stream[B] = {
    tails(st)
        .map(tailStream => foldRight(tailStream, acc)((a,acc) => f(a, acc)))
        .append(Stream.cons(acc, End))
  }

  def scanRight_2[A, B](st: Stream[A], acc: => B)(f: (A, => B) => B) : Stream[B] = {
    foldRight(st, Stream(acc))((a, res) => res.take(1).map(b => f(a, b)).append(res))
  }

}


