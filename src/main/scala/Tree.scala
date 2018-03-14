sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]) : Int = {
    tree match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }
  }

  def max[A](tree: Tree[A])(fmax: (A, A) => A) : A  = {
    tree match {
      case Branch(l,r) => fmax(max(l)(fmax), max(r)(fmax))
      case Leaf(a) => a
    }
  }

  def map[A,B](tree: Tree[A])(f: A => B) : Tree[B]  = {
    tree match {
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
    }
  }

  def fold1[A,B](tree: Tree[A], acc: B)(f: (A, B) => B) : B = {
    tree match {
      case Branch(l,r) => fold1(r, fold1(l, acc)(f))(f)
      case Leaf(a) => f(a, acc)
    }
  }

  def fold2[A,B](tree: Tree[A])(fleaf: (A => B))(fbranch: (B, B) => B) : B = {
    tree match {
      case Branch(l,r) => fbranch(fold2(l)(fleaf)(fbranch), fold2(r)(fleaf)(fbranch))
      case Leaf(a) => fleaf(a)
    }
  }

  def depth1[A](tree: Tree[A]) : Int = {
    def go(tree: Tree[A], level: Int) : Int = {
      tree match {
        case Branch(l, r) => Math.max(go(l, level + 1), go(r, level + 1))
        case Leaf(_) => level;
      }
    }
    go(tree,1)
  }

  def depth2[A](tree: Tree[A]) : Int = {
    tree match {
      case Branch(l, r) => Math.max(1 + depth2(l), 1 + depth2(r))
      case Leaf(_) => 1;
    }
  }
}
