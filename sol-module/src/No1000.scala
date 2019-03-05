import scala.collection.mutable
object No1000 {
  case class Stones(stones: List[Int], K: Int)
  val mem = mutable.HashMap[Stones, Int]()

  def mergeStones_bf(stones: Array[Int], K: Int): Int = {
    val thisStone = Stones(stones.toList, K)
    if(mem.contains(thisStone)){
      //println(s"found in mem: Stone ${thisStone.stones}")
      return mem(thisStone)
    }
    if (K < 2 || (K > 2 && stones.length % (K - 1) != 1)) {
      mem(thisStone) = -1
      return -1
    }
    if(stones.length == K){
      mem(thisStone) = stones.sum
      return stones.sum
    }
    val ans = (for(x <- 0 to stones.length - K) yield {
      val sum = stones.slice(x,x+K).sum
      val newStone = stones.slice(0,x) ++ Array(sum) ++ stones.slice(x+K, stones.length)
      //println(s"sum: $sum, newStone: ${newStone.toList}")
      sum + mergeStones_bf(newStone, K)
    }).min
    mem(thisStone) = ans
    ans
  }


  def mergeStones_dp(stones: Array[Int], K: Int): Int = {
    val INF = 1e9.toInt
    val N = stones.length
    if ( N <= 0 || K<=1) return -1
    if (N==1) return 0

    if((N-1)%(K-1) != 0) return -1

    // f[i][j][k]: the cost to make from i to j result in k piles,
    val f = Array.ofDim[Int](30,30,31)
    val sum = Array.ofDim[Int](30)
    for (i <- 0 until N) {
      sum(i) = stones(i) + (if(i==0) 0 else sum(i-1))
      f(i)(i)(1) = 0
      for ( k<- 2 to K)
        f(i)(i)(k) = INF
    }

    println(sum.toList)

    for (len<- 2 to N) {
      var i = 0
      for ( j<- len-1 until N) {
        for (k<-2 to K) {
          f(i)(j)(k) = INF
          for (t<-i until j)
            f(i)(j)(k) = f(i)(j)(k) min f(i)(t)(k-1)+f(t+1)(j)(1)// 理解这个公式, 分成(k-1)和1  而不是 k-i和i
          println(s"i: $i, j: $j, k: $k, f(i,j,k):${ f(i)(j)(k)}")
        }
        f(i)(j)(1) = f(i)(j)(K) + sum(j)-(if(i>0) sum(i-1) else 0)
        println(s"i: $i, j: $j, f(i)(j)(1) : ${f(i)(j)(1) }")
        i+=1
      }
    }
    if(f(0)(N-1)(1) >= INF) -1 else f(0)(N-1)(1)
  }


}
