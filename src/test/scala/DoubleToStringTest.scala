import org.scalatest.FunSpec

class DoubleToStringTest extends FunSpec {

  describe("double to string function ") {
    describe("applied to empty list") {
      val emptyList = List[Double]()
      it("should return empty list") {
        assertResult(Transform.map(emptyList)(a => a.toString)){
          List()
        }
      }
    }

    describe("applied to single element list") {
      val singleList = List(1.0d)
      it("should return 1") {
        assertResult(Transform.map(singleList)(a => a.toString)){
         List("1.0")
        }
      }
    }

    describe("applied list of list") {
      val someList = List(1.0d,2.0d,3.0d)
      it("should return all element one greater") {
        assertResult(Transform.map(someList)(a => a.toString)){
         List("1.0","2.0", "3.0") }
      }
    }
  }
}
