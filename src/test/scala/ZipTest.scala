import org.scalatest.FunSpec

class ZipTest extends FunSpec{

  describe("zip function ") {
    describe("applied to empty list") {
      val emptyList = List[Int]()
      it("should return empty list") {
        assertResult(Zip.zip(List(), List())((a,b) => a)){
          List()
        }
      }
    }

    describe("applied to shortest list") {
      val first = List(1);
      val second = List(2,3);
      it("should return 1") {
        assertResult(Zip.zip(first,second)((a,b) => a+b)){
          List(3)
        }
      }
    }

  }

}
