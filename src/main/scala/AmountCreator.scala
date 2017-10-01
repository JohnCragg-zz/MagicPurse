import scala.util.{Success, Try}
import scala.util.matching.Regex

object AmountCreator {
  type Purse = Seq[(Coin, Int)]

  val singleSlashPattern: Regex = "(.+)/(.+)".r
  val doubleSlashPattern: Regex = "(.+)/(.+)/(.+)".r

  def createAmount(input: String): Purse = input match {
    case doubleSlashPattern(pounds, "-", "-") => createPounds(pounds)
    case doubleSlashPattern(pounds, shillings, "-") => createPounds(pounds) ++ createShillings(shillings)
    case doubleSlashPattern(pounds, "-", pennies) => createPounds(pounds) ++ createPennies(pennies)
    case doubleSlashPattern(pounds, shillings, pennies) => createPounds(pounds) ++ createShillings(shillings) ++ createPennies(pennies)
    case singleSlashPattern(shillings, "-") => createShillings(shillings)
    case singleSlashPattern("-", pennies) => createPennies(pennies)
    case singleSlashPattern(shillings, pennies) => createShillings(shillings) ++ createPennies(pennies)
  }

  def canPermutationBeSplit(p: Map[Coin, Int]): Boolean = p.values.sum % 2 == 0

  def createPounds(p: String): Purse = Seq((Pound, p.toInt))

  def createShillings(s: String): Purse = s.toInt match {
    case number if number < 5 => toFlorinOrShillings(number)
    case number => Seq((HalfCrown, 2 * (number / 5))) ++ toFlorinOrShillings(number % 5)
  }

  def toFlorinOrShillings(i: Int) = {
    i match {
      case j if j % 2 == 0 => Seq((Florin, j / 2))
      case j if j == 1 => Seq((Shilling, 1))
      case j => Seq((Florin, j / 2), (Shilling, 1))
    }
  }

  def createPennies(p: String): Purse = Try(p.toInt) match {
    case Success(i) => toPennyThreepenceOrSixpence(i)
    case Success(i) => toPennyThreepenceOrSixpence(i)
    case _ =>
      val j = (p.toDouble - 0.5).toInt.toString
      createPennies(j) ++ Seq((HalfPenny, 1))
  }

  def toPennyThreepenceOrSixpence(i: Int) = if (i < 6) toPennyOrThreepence(i)
  else {
    val remainder = i % 6
    val numberOfSixpence = (i - i % 6) / 6
    if (remainder == 0) Seq((Sixpence, numberOfSixpence))
    else Seq((Sixpence, numberOfSixpence)) ++ toPennyOrThreepence(remainder)
  }

  def toPennyOrThreepence(i: Int) = if (i < 3) Seq((Penny, i))
  else if (i == 3) Seq((Threepence, 1))
  else Seq((Threepence, 1), (Penny, i % 3))

}
