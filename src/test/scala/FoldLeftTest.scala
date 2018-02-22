import org.scalatest.FunSpec

class FoldLeftTest extends FunSpec {

  describe("foldLeft function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return zero") {
        assertResult(FoldImplementation.foldLeft(emptyList, 0)((b,a)=>b+1)){
          0
        }
      }
    }

    describe("applied to single element list") {
      val singleElemnt = 1 :: Nil;
      it("should return 1") {
        assertResult(FoldImplementation.foldLeft(singleElemnt, 0)((b,a)=>b+1))
        { 1 }
      }
    }

    describe("applied to 4 element list") {
      val list = (1 to 1000000).toList
      it("should return 4") {
        assertResult(FoldImplementation.foldLeft(list, 0)((b,a)=>b+1))
        { 1000000 }
      }
    }
  }
}
