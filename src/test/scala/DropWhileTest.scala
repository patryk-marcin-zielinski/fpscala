import org.scalatest.FunSpec

class DropWhileTest extends FunSpec{

  describe("Drop while function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return empty list") {
        assertResult(Drop.drop(emptyList, 100)){
          Nil
        }
      }
    }

    describe("drop element until condition true") {
      val list = List(1, 2, 3, 4);
      it("should return list with remove head element") {
        assertResult(DropWhile.dropWhile(list,(x: Int) => x < 3)) { List(3, 4) }
      }
    }

    describe("drop all element ") {
      val list = List(1, 2, 3, 4);
      it("when all element match") {
        assertResult(DropWhile.dropWhile(list,(x: Int) => x < 10)) { Nil }
      }
    }
  }

}
