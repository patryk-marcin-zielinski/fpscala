import org.scalatest.FunSpec

class OptionTest extends FunSpec{

  describe("Optional map") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should be empty") {
        assertResult(emptyOption.map(a => a + 1)) {
          None
        }
      }
    }

    describe("applied to Some option") {
      val emptyOption = Some(1);
      it("should add one") {
        assertResult(emptyOption.map(a => a + 1)) {
          Some(2)
        }
      }
    }
  }

}
