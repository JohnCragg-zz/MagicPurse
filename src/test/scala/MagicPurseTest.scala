import org.scalatest.{FunSpec, Matchers}

class MagicPurseTest extends FunSpec with Matchers {

  describe("magic purse") {

    it("should work") {
      val purse = new MagicPurse("5/-/-")
      val amount = purse.initialCoins
      purse.combinations(amount) shouldBe 0
    }
  }








}