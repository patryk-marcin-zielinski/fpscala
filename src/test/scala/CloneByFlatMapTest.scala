import org.scalatest.FunSpec

class CloneByFlatMapTest extends FunSpec {

  describe("transform function ") {
    describe("applied to empty list") {
      val emptyList = List[Int]()
      it("should return empty list") {
        assertResult(FlatMap.flatMap1(emptyList)(i => List(i,i))){
          List()
        }
        assertResult(FlatMap.flatMap2(emptyList)(i => List(i,i))){
          List()
        }
      }
    }

    describe("applied to single element list") {
      val singleList = List(1)
      it("should return List(1,1)") {
        assertResult(FlatMap.flatMap1(singleList)(i => List(i,i))){
         List(1,1)
        }
        assertResult(FlatMap.flatMap2(singleList)(i => List(i,i))){
          List(1,1)
        }
      }
    }

    describe("applied list of list") {
      val someList = List(1,2,3)
      it("should clone element list") {
        assertResult(FlatMap.flatMap1(someList)(i => List(i,i))){
          List(1,1,2,2,3,3)
        }
        assertResult(FlatMap.flatMap2(someList)(i => List(i,i))){
          List(1,1,2,2,3,3)
        }
      }
    }
  }
}
