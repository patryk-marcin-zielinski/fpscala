import org.scalatest.{FlatSpec, FunSpec, FunSuite}

class TailTest extends FunSpec {

  describe("Tail function ") {
    describe("applied to empty list") {
      val emptyList = List();
      it("should return empty list") {
        assertResult(Tail.tail(emptyList)){
          Nil
        }
      }
    }

    describe("applied to single element list") {
      val singleElemnt = 1 :: Nil;
      it("should return empty list") {
        assertResult(Tail.tail(singleElemnt)) { Nil }
      }
    }

    describe("applied to many element list") {
      val list = List(1, 2, 3, 4);
      it("should return list with remove head element") {
        assertResult(Tail.tail(list)) { List(2,3,4) }
      }
    }
  }
}
