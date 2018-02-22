import org.scalatest.FunSpec

class LengthTest extends FunSpec {

  describe("length function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return zero") {
        assertResult(Length.length(emptyList)){
          0
        }
      }
    }

    describe("applied to single element list") {
      val singleElemnt = 1 :: Nil;
      it("should return 1") {
        assertResult(Length.length(singleElemnt)) { 1 }
      }
    }

    describe("applied to 4 element list") {
      val list = List(1, 2, 3, 4);
      it("should return 4") {
        assertResult(Length.length(list)) { 4 }
      }
    }
  }
}
