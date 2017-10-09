import org.scalatest.{FunSpec, FunSuite, Matchers}

class MPTest extends FunSpec with Matchers {

  describe("x") {

    it("y") {
      val mp = new MP(5, Array(1, 2))
      mp.evenPermutations shouldBe List(List((1,1,1,2)))
    }
  }

}
