class MagicPurse(input: String) {
  val initialCoins: Map[Coin, Int] = Map(
      Pound -> 0,
      HalfCrown -> 0,
      Florin -> 0,
      Shilling -> 0,
      Sixpence -> 0,
      Threepence -> 0,
      Penny -> 0,
      HalfPenny -> 0
  ) ++ AmountCreator.createAmount(input).toMap

  val inHalfPennies  : Int = initialCoins.foldLeft(0){
    (acc, c) => c._1 match {
      case Pound => acc + 480*c._2
      case HalfCrown => acc + 60*c._2
      case Florin => acc + 48*c._2
      case Shilling => acc + 24*c._2
      case Sixpence => acc + 12*c._2
      case Threepence => acc + 6*c._2
      case Penny => acc + 2*c._2
      case HalfPenny => acc + c._2
    }
  }

  def combinations(purse : Map[Coin , Int], acc: Int = 0) : Int = purse match {
      case p if p(HalfPenny) == inHalfPennies => if (inHalfPennies % 2 == 0) acc + 1 else acc
      case p if purse.values.sum % 2 == 0 => combinations(decomposeHighestCoin(purse), acc + 1)
      case _ => combinations(decomposeHighestCoin(purse), acc)
    }




  val splitCoin : (Map[Coin , Int], Coin, Coin, Int) => Map[Coin , Int] = (purse, coin, smallerCoin, conversion) => purse(coin) match {
    case n if n > 0 => purse ++ Map(coin -> (n - 1)) ++ Map(smallerCoin -> (purse(smallerCoin)+conversion))
    case _ => throw new RuntimeException(s"No coin $coin to convert")
  }

  val splitHalfCrown : Map[Coin, Int] => Map[Coin, Int] = purse => purse(HalfCrown) match {
    case n if n > 0 => purse ++ Map(HalfCrown -> (n - 1)) ++ Map(Florin -> (purse(Florin)+1)) ++ Map(Sixpence -> (purse(Sixpence) + 1 ))
  }

  def decomposeHighestCoin(purse: Map[Coin, Int]) = purse match {
    case p if p(Pound) > 0 => splitCoin(purse, Pound, HalfCrown, 8 )
    case p if p(HalfCrown) > 0 => splitHalfCrown(purse)
    case p if p(Florin) > 0 => splitCoin(purse, Florin, Shilling, 2 )
    case p if p(Shilling) > 0 => splitCoin(purse, Shilling, Sixpence, 2 )
    case p if p(Sixpence) > 0 => splitCoin(purse, Sixpence, Threepence, 2 )
    case p if p(Threepence) > 0 => splitCoin(purse, Threepence, Penny, 3 )
    case p if p(Penny) > 0 => splitCoin(purse, Penny, HalfPenny, 2)
  }

}
