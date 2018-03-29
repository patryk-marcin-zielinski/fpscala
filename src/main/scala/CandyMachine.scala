sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.map {
      case Coin => {
        println("COIN")
        State[Machine, (Int, Int)](machine => {
          println("COINS ADDING")
          println(machine)
          val tuple = ((machine.coins+1, machine.candies), Machine(machine.locked, machine.candies, machine.coins+1))
          print("Result")
          println(tuple)
          tuple
        })
      }
      case Turn => {
        println("TURN")
        State[Machine, (Int, Int)](machine => {
          println("CANDY SUBSTRACTING")
          println(machine)
          val tuple  = ((machine.coins, machine.candies-1), Machine(machine.locked, machine.candies-1, machine.coins))
          print("Result")
          println(tuple)
          tuple
        })
      }
    }
    .foldLeft(State[Machine, (Int, Int)](machine => ((machine.coins, machine.candies),machine)))((acc, state) => {
      State.map2(acc, state)((res1, res2) => {
        print("ACC = ")
        println(res1)
        print("Element = ")
        println(res2)
        res2
      })
    })
  }

  def lock(state : State[Machine, (Int, Int)]) = {
    modify(state)(machine => Machine(true, machine.candies, machine.coins))
  }

  def unlock(state : State[Machine, (Int, Int)]) = {
    modify(state)(machine => Machine(false, machine.candies, machine.coins))
  }

  def modify(state : State[Machine, (Int, Int)])(f: Machine => Machine) : State[Machine, (Int, Int)] = {
    state.get
      .flatMap(machine => state.set(f(machine))
        .map((_) => (machine.coins, machine.candies))
      )
  }


}


