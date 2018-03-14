import org.scalatest.FunSpec

class JoinTest extends FunSpec {

  describe("join function ") {
    describe("applied to empty list") {
      val emptyList = List(List())
      it("should return zero") {
        assertResult(Join.join(emptyList)){
          List()
        }
      }
    }

    describe("applied to single element list") {
      val singleList = List(List(1,2,3))
      it("should return 1") {
        assertResult(Join.join(singleList)){
         List(1,2,3)
        }
      }
    }

    describe("applied list of list") {
      val someList = List(List(1,2), List(4,3), List(7,6))
      it("should return 4") {
        assertResult(Join.join(someList)){
         List(1,2,4,3,7,6) }
      }
    }
  }
}
