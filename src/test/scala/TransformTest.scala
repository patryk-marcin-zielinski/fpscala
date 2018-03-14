import org.scalatest.FunSpec

class TransformTest extends FunSpec {

  describe("transform function ") {
    describe("applied to empty list") {
      val emptyList = List[Int]()
      it("should return empty list") {
        assertResult(Transform.map(emptyList)(a => a+1)){
          List()
        }
      }
    }

    describe("applied to single element list") {
      val singleList = List(1)
      it("should return 1") {
        assertResult(Transform.map(singleList)(a => a+1)){
         List(2)
        }
      }
    }

    describe("applied list of list") {
      val someList = List(1,2,3)
      it("should return all element one greater") {
        assertResult(Transform.map(someList)(a => a+1)){
         List(2,3,4) }
      }
    }
  }
}
