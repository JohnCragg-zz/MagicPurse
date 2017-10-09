type Permutation = List[Int]
val coins = Array(1, 2)
val numCoins = coins.length

val target = 5

def calculatePermutations(permutationsIndexedByAmount: Array[List[Permutation]],
                          amount: Int,
                          target: Int) = {
  val entryForAmount = for {
    i <- (1 until target - amount).toList
  } yield crossProduct(permutationsIndexedByAmount(i), permutationsIndexedByAmount(target - i))

  val flattened = entryForAmount.flatten
  permutationsIndexedByAmount :+ flattened

}

def crossProduct(a: List[Permutation], b: List[Permutation]) = for {
  la <- a
  lb <- b
} yield la ::: lb


val allPermutations = (1 until target).foldLeft(Array(List(List(1)))) {
  (acc, amount) => calculatePermutations(acc, amount, target)
}
val targetPermutation = allPermutations(target)
val evenPermutations = targetPermutation.filter(_.length % 2 == 0)