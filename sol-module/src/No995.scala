/**
  * Created by lilisun on 2/22/19.
  */
object No995 {

  def minKBitFlips(A: Array[Int], K: Int): Int = solverOptGreedy(A, K)

  def solverOptGreedy(A: Array[Int], K: Int): Int = {
    val N = A.length
    val hint = (0 until N).map(x => 0).toArray
    var ans = 0; var flip = 0

    for (i <- 0 until N) {
      flip ^= hint(i)
      if (A(i) == flip) {
        ans+= 1
        if (i + K > N) return -1
        flip ^= 1
        if (i + K < N) hint(i + K) ^= 1
      }
    }
    ans
  }

  def minKBitFlips(A: Array[Int], K: Int,  acc: Int): Int = {
    if(K == A.length) {
      if(A.mkString == A.map(x => 1).mkString)  acc
      else if(A.mkString == A.map(x => 0).mkString)  1+acc
      else -1
    }else{
      if(A(0) == 1) minKBitFlips(A.tail, K, acc)
      else minKBitFlips(flip(A.tail, K), K, acc + 1)
    }
  }

  def flip(A: Array[Int], K: Int): Array[Int] = {
    val B = A
    for(x <- 0 until K - 1) B(x) = if(B(x) == 1) 0 else 1
    B
  }

  def main(args: Array[String]): Unit = {
    val t = Array(0,0,0,1,0,1,1,0)
    println(minKBitFlips(t,3) ==3)
    println(minKBitFlips(Array(1,1,0),2) == -1)
    println(minKBitFlips(Array(0,1,0),1) == 2)

  }

}
