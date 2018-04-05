import org.scalatest.FunSpec

/*
- Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
- Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
- Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
- A machine that’s out of candy ignores all inputs.
*/


class CandyMachineTest extends FunSpec{

  describe("Candy machine with 1 candies and 0 coins") {
    val candyMachine = Machine(true, 1,0)
    describe("when single coin dropped") {
      val result = Machine.simulateMachine(List(Coin)).run(candyMachine)
      it("should be unlock, and coin added") {
        assertResult(result._1) {(1,1)}
        assertResult(result._2.locked) { false }
      }
    }

    describe("when two coins drop") {
      val result = Machine.simulateMachine(List(Coin, Coin)).run(candyMachine)
      it("should secend coin does nothing") {
        assertResult(result._1) {(1,1)}
      }
    }

    describe("when drop a coin and turn knob") {
      val state = Machine.simulateMachine(List(Coin, Turn))
      val result = state.run(candyMachine)
      it("should take coing and drop candy") {
        assertResult(result._1) {(1,0)}
        assertResult(result._2.locked) {true}
      }
    }
  }

  describe("Candy machine with 5 candies, 10 coins"){
    val candyMachine = Machine(true, 5,10)
    describe("when pay and take for 4 candies") {
      val state = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      val result = state.run(candyMachine)
      it("should have 14 coins and 1 candies") {
        assertResult(result._1) {(14,1)}
        assertResult(result._2.locked) {true}
      }
    }
  }
}
