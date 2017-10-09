import org.scalatest.{FunSpec, Matchers}

class MagicPurseTest extends FunSpec with Matchers {

  describe("magic purse") {

    it("should return 141 for one shilling") {
      val purse = new MagicPurse("1/-")
      val amount = purse.initialCoins
      purse.combinations(amount) shouldBe 0
    }

    it("should return 2377 for one Florin ") {
      val purse = new MagicPurse("2/-")
      val amount = purse.initialCoins
      purse.combinations(amount) shouldBe 2377
    }



  }








}