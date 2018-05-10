import org.scalatest.FunSpec

class ParTest extends FunSpec {


  describe("Async function") {
    val asyncFs = Par.asyncF2[Int,Int](a => {
      Thread.sleep(1000)
      1
    })
  }

  describe("Par filter") {

  }

}
