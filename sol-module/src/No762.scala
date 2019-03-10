/**
  * Created by lilisun on 3/10/19.
  */
object No762 {

  def countPrimeSetBits(L: Int, R: Int): Int = {
    (L to R).count(primeOnes)
  }
  def primeOnes(n: Int): Boolean = {
    isPrime(countOnes(n, 0))
  }

  def countOnes(n: Int, acc: Int): Int = {
    if(n == 0) acc
    else n % 2 match {
      case 1 => countOnes(n / 2, acc+1)
      case 0 => countOnes(n / 2, acc)
    }
  }

  def isPrime(n: Int): Boolean = {
    val primeSet = Set(2,3,5,7,11,13,17,19, 23, 29)
    primeSet.contains(n)
  }

  def main(args: Array[String]): Unit = {
      println(countPrimeSetBits(10, 15))
  }

}
