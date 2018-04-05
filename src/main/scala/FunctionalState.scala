

trait RNG {
  def nextInt : (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S,B] = {
    flatMap(a => State.unit[S,B](f(a)))
  }

  def flatMap[B] (g: A => State[S,B]): State[S,B] = {
    State(state => {
      val (a, stateA) = this.run(state)
      g(a).run(stateA)
    })
  }
}


object State {

  def unit[S, A](a: A): State[S, A] = State(state => (a,state))

  def map2[S,A,B,C](ra: State[S,A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    ra.flatMap(a => rb.map(b => f(a,b)))
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight[State[S, List[A]]](State(state => (List[A](), state)))((stateA, stateB) => stateB.flatMap(acc => stateA.map(a => a :: acc)))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](newState: S) : State[S, Unit] = State (_ => ((), newState))
}


object Rand {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    lazy val (intRandom, rand) = rng.nextInt;
    if(Int.MinValue == intRandom) (math.abs(intRandom + 1), rand) else (math.abs(intRandom), rand);
  }

  def double(rng: RNG): (Double, RNG) = {
    lazy val nonNegative = nonNegativeInt(rng)
    val range = math.max(0, nonNegative._1 - 1)
    val divide = range.toDouble / Int.MaxValue.toDouble;
    (divide, nonNegative._2)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = {
    val rnd : Rand[Int] = _.nextInt
    map(rnd)(a => a.toDouble / Int.MaxValue.toDouble)(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rnd => {
      val (a, arnd) = ra(rnd)
      val (b, brnd) = rb(arnd)
      (f(a,b), brnd)
    }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, intRng) = rng.nextInt
    val (d, doubleRng) = double(intRng)
    ((i, d), doubleRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val tuple = intDouble(rng)
    (tuple._1.swap, tuple._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, d1rng) = double(rng)
    val (d2, d2rng) = double(d1rng)
    val (d3, d3rng) = double(d2rng)
    ((d1, d2, d3), d3rng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (Nil, rng)
    else {
      val intRandom = rng.nextInt;
      val rngToTuple = ints(count - 1)(intRandom._2)
      (intRandom._1 :: rngToTuple._1, intRandom._2)
    }
  }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill[Rand[Int]](count)((rnd => rnd.nextInt) : Rand[Int]))(rng);
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))


  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
   fs.foldRight((rnd => (List[A](), rnd)): Rand[List[A]])((rnd, acc) => map2(rnd, acc)((a, list) => a :: list))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rnd => {
      val (a, rnda) = f(rnd)
      g(a)(rnda)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) (i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })
}

