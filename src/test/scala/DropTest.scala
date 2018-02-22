import org.scalatest.FunSpec

class DropTest extends FunSpec{

  describe("Drop head function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return empty list") {
        assertResult(Drop.drop(emptyList, 100)){
          Nil
        }
      }
    }

    describe("drop more than element list") {
      val list = List(1, 2, 3, 4);
      it("should return list with remove head element") {
        assertResult(Drop.drop(list, 5)) { Nil }
      }
    }

    describe("drop half element of list") {
      val list = List(1, 2, 3, 4);
      it("should return list with remove head element") {
        assertResult(Drop.drop(list, list.length / 2)) { List(3,4) }
      }
    }
  }

}
