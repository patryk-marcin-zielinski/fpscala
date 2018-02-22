import org.scalatest.FunSpec

class InitTest extends FunSpec{

  describe("Init function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return empty list") {
        assertResult(Init.init(emptyList)){
          Nil
        }
      }
    }

    describe("applied to five element list") {
      val fiveElementList = List(1,2,3,4,5);
      it("should return list without last element") {
        println(Init.init(fiveElementList))
        assertResult(Init.init(fiveElementList)){
          List(1,2,3,4)
        }
      }
    }
  }

}
