sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs
        .map({
          case Coin => insertCoin
          case Turn => takeCandy
        })
        .foldLeft[State[Machine, (Int, Int)]](State(s => ((s.coins, s.candies), s)))((acc, s) => acc.flatMap(_ => s))
  }

  def insertCoin : State[Machine, (Int, Int)] = {
    for {
      toLock <- State.get
      _ <- State.set(addCoin(toLock))
      machine <- State.get
    } yield (machine.coins, machine.candies)
  }

  def takeCandy : State[Machine, (Int, Int)]  = {
    for {
      toUnlock <- State.get
      _ <- State.set(decreaseCandy(toUnlock))
      machine <- State.get
    } yield (machine.coins, machine.candies)
  }

  def addCoin(machine: Machine) : Machine = machine match {
    case Machine(false, _, _) => machine
    case Machine(_, 0, _) => machine
    case Machine(_, candies, coins) => Machine(locked = false, candies, coins + 1)
  }

  def decreaseCandy(machine: Machine): Machine = machine match {
    case Machine(true, _, _) => machine
    case Machine(_, 0, _) => machine
    case Machine(_, candies, coins) => Machine(locked = true, candies - 1, coins)
  }

}


