import org.scalatest.FunSpec

class ReverseTest extends FunSpec {

  describe("reverse function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return zero") {
        assertResult(ReverseList.reverse1(emptyList)){Nil}
        assertResult(ReverseList.reverse2(emptyList)){Nil}
      }
    }

    describe("applied to single element list") {
      val singleElemnt = 1 :: Nil;
      it("should return 1") {
        assertResult(ReverseList.reverse1(singleElemnt)) { List(1) }
        assertResult(ReverseList.reverse2(singleElemnt)) { List(1) }
      }
    }

    describe("applied to 4 element list") {
      val list = List(1, 2, 3, 4);
      it("should return 4") {
        assertResult(ReverseList.reverse1(list)) { List(4,3,2,1) }
        assertResult(ReverseList.reverse2(list)) { List(4,3,2,1) }
      }
    }
  }
}
