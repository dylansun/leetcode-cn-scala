/**
  * Created by lilisun on 3/7/19.
  */
object No974 {

  def subarraysDivByK(A: Array[Int], K: Int): Int = {
  calculate(count(calCumArr(A, K), K), K)
  }

  def calculate(A: Array[Int], K: Int): Int = {
    var res = 0
    for(x <- A) res +=  x * (x -1) /2
    res + A(0) // 余数为0的点, 可以自己是一段
  }
  def count(A:Array[Int], K: Int): Array[Int] = {
    val ans = Array.ofDim[Int](K)
    for(x <- A) ans(x) = ans(x) + 1
    ans
  }
  // cum(i) in 0 to K-1
  def calCumArr(A:Array[Int], K: Int): Array[Int] = {
    val cum = Array.ofDim[Int](A.length)
    for(i<- A.indices){
      i match {
        case 0 => cum(i) = (A(i) % K + K) % K
        case _ => cum(i) = ((A(i) + cum(i-1)) %K +K) % K
      }
    }
    cum
  }

  def testMod(): Unit = {
    (-10 to 10).foreach(x => println(s"x: $x, res: ${x % 3}"))
  }
  def test(): Unit = {
    val a = Array(4,5,0,-2,-3,1)
    val K = 5
    println(subarraysDivByK(a, K))
  }
  def main(args: Array[String]): Unit = {
    test()

  }
}
