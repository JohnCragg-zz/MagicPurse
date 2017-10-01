import org.scalatest.{FunSpec, Matchers}

class AmountCreatorTest extends FunSpec with Matchers {

  import AmountCreator._

  describe("create amount") {
    it("can extract pounds with dashes") {
      createAmount("""1/-/-""") shouldBe Seq((Pound, 1))
    }
    it("can extract shillings from a single slashed input with dashes") {
      createAmount("1/-") shouldBe Seq((Shilling, 1))
    }
    it("can extract pounds shillings and pence from a double slashed and no dashes") {
      createAmount("1/1/1") shouldBe Seq((Pound, 1), (Shilling, 1), (Penny, 1))
    }

    it("between 3 and 5 pence creates threepence and left over pence") {
      createAmount("-/3") shouldBe Seq((Threepence, 1))
      createAmount("-/4") shouldBe Seq((Threepence, 1), (Penny, 1))
      createAmount("-/5") shouldBe Seq((Threepence, 1), (Penny, 2))
    }
    it("between 6 and 8 pence creates sixpence and left over pence") {
      createAmount("-/6") shouldBe Seq((Sixpence, 1))
      createAmount("-/7") shouldBe Seq((Sixpence, 1), (Penny, 1))
      createAmount("-/8") shouldBe Seq((Sixpence, 1), (Penny, 2))
    }
    it("between 9 and 11 pence creates sixpence, threepence and left over pence") {
      createAmount("-/9") shouldBe Seq((Sixpence, 1), (Threepence, 1))
      createAmount("-/10") shouldBe Seq((Sixpence, 1), (Threepence, 1), (Penny, 1))
      createAmount("-/11") shouldBe Seq((Sixpence, 1), (Threepence, 1), (Penny, 2))
    }
    it("2 shillings creates a Florin") {
      createAmount("2/-") shouldBe Seq((Florin, 1))
    }
    it("5 shillings creates 2 half crowns") {

    }
  }
}
