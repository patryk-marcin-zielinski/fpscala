import org.scalatest.{FunSpec, FunSuite}

class SetHeadTest extends FunSpec {

  describe("Set head function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return empty list") {
        assertResult(SetHead.replaceHead(1, emptyList)){
          Nil
        }
      }
    }

    describe("applied to many element list") {
      val list = List(1, 2, 3, 4);
      it("should return list with remove head element") {
        assertResult(SetHead.replaceHead(100, list)) { List(100, 2,3,4) }
      }
    }
  }

}
