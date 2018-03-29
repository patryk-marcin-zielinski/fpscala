import org.scalatest._

class StateTest extends FunSpec {

  describe("RNG non negative random function") {

    describe("for Int.MinValue  ") {

      val result = Rand.nonNegativeInt(new RNG {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      })

      it("should generate a Int.MaxValue") {
        assertResult(result._1) { Int.MaxValue }
      }
    }
  }


  describe("RNG for double function") {

    describe("for Int.MinValue  ") {

      val result = Rand.double(new RNG {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      })

      it("should generate a 0.9999") {
        assertResult(result._1) { (Int.MaxValue - 1).toDouble/Int.MaxValue.toDouble }
      }
    }
  }


  describe("RNG for ints function") {

    describe("for 10 values") {

      val ones = new RNG {
        override def nextInt: (Int, RNG) = (1, this)
      }

      val ints = Rand.ints(10)(ones)

      val intsViaSequence = Rand.intsViaSequence(10)(ones)

      it("should generate a List(1,1,1,1,1,1,1,1,1,1)") {
        assertResult(ints._1) { List(1,1,1,1,1,1,1,1,1,1) }
        assertResult(intsViaSequence._1) { List(1,1,1,1,1,1,1,1,1,1) }
      }
    }
  }

  describe("RNG for noneNegativeLessThan") {

    describe(" under 1000") {

      val oneH = new RNG {
        override def nextInt: (Int, RNG) = (100, this)
      }

      val lessThan = Rand.nonNegativeLessThan(1000)(oneH)

      it("should generate a random value 100") {
        assertResult(lessThan._1) { 100 }
      }
    }

    describe(" random value above 1000") {

      val oneH = new RNG {
        override def nextInt: (Int, RNG) = (1200, this)
      }

      val lessThan = Rand.nonNegativeLessThan(1000)(oneH)

      it("should generate a random value 200") {
        assertResult(lessThan._1) { 200 }
      }
    }
  }

}
