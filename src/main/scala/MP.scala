class MP(target: Int, coins: Array[Int]) {
  type Permutation = List[Int]

  val numCoins: Int = coins.length

  def calculatePermutations(permutationsIndexedByAmount: Array[List[Permutation]],
                            amount: Int,
                            target: Int): Array[List[Permutation]] = {
    val entryForAmount = for {
      i <- (0 until amount - 1).toList
    } yield if (i == 0) List(List(amount))
    else crossProduct(permutationsIndexedByAmount(i), permutationsIndexedByAmount(amount - i))

    val flattened = entryForAmount.flatten
    permutationsIndexedByAmount :+ flattened

  }

  def crossProduct(a: List[Permutation], b: List[Permutation]): List[Permutation] = for {
    la <- a
    lb <- b
  } yield la ::: lb


  val allPermutations: Array[List[Permutation]] = (1 until target + 1).foldLeft(Array(List(List.empty[Int]))) {
    (acc, amount) => if (amount == 1) Array(List(List(1)), List(List(1))) else calculatePermutations(acc, amount, target)
  }
  val targetPermutation: List[Permutation] = allPermutations(target)
  val evenPermutations: List[Permutation] = targetPermutation.filter(_.length % 2 == 0)
  val numberOfEvenPermutations : Int = evenPermutations.size

}

