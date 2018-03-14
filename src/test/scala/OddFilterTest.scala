import org.scalatest.FunSpec

class OddFilterTest extends FunSpec {

  describe("filter function ") {
    describe("applied to empty list") {
      val emptyList = List[Int]()
      it("should return empty list") {
        assertResult(Filter.filter(emptyList)(a => a % 2 == 0)){
          List()
        }
      }
    }

    describe("applied to single element list") {
      val singleList = List(2)
      it("should return 1") {
        assertResult(Filter.filter(singleList)(a => a % 2 == 0)){
         List(2)
        }
      }
    }

    describe("applied list of list") {
      val someList = List(1,2,3,4)
      it("should return all even elements") {
        assertResult(Filter.filter(someList)(a => a % 2 == 0)){
         List(2,4) }
      }
    }
  }
}
