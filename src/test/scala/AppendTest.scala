import org.scalatest.FunSpec

class AppendTest extends FunSpec {

  describe("append function ") {
    describe("applied to empty list") {
      val emptyList = List()
      val someList = List(1,2,3)
      it("should return zero") {
        assertResult(Append.append(someList, emptyList)){
          someList
        }
      }
    }

    describe("applied to single element list") {
      val singleElemnt = 1 :: Nil
      val someList = List(1,2,3)
      it("should return 1") {
        assertResult(Append.append(someList, singleElemnt)){
         List(1,2,3,1)
        }
      }
    }

    describe("applied to 4 element list") {
      val reverseElement = 3 :: 2 :: 1 :: Nil
      val someList = List(1,2,3)
      it("should return 4") {
        assertResult(Append.append(someList, reverseElement))
        { List(1,2,3,3,2,1) }
      }
    }
  }
}
