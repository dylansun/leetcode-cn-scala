/**
  * Created by lilisun on 3/3/19.
  */

import scala.collection.mutable
object Contest126 {

  def commonChars(A: Array[String]): List[String] = {
    if(A.length == 0) return List[String]()
    if(A.length == 1) return A(0).toCharArray.map(x => x.toString).toList
    val m = mutable.HashMap[Char,Int]()
    val keys = A.foldLeft(('a' to 'z').toSet)( (keys, x) => x.toCharArray.toSet intersect keys )
    for(x <- A.head.toCharArray if keys.contains(x)) m.put(x, m.getOrElse(x,0)+1)
    A.tail.foreach(str => for(key <- m.keySet)m(key) = m(key) min str.count(_==key))

    var ans = List[String]()
    for(k <- m.keySet) for(i <- 0 until m(k)) ans = k.toString::ans
    ans
  }


  def longestOnes(nums: Array[Int], k: Int): Int = {
    var res = 0
    var zero = 0
    var left = 0
    for(right <- nums.indices ){
      if(nums(right) == 0) zero +=1
      while(zero > k){
        if(nums(left) == 0)left += 1
        zero -= 1
      }
      res = res max right-left +1
    }
    res
  }

    def isValid(S: String): Boolean = {
      if(S == "") true
      else if(S.replace("abc","") == S) false
      else isValid(S.replace("abc",""))
    }

  def mergeStones_bf(stones: Array[Int], K: Int): Int = {
    if (K < 2 || (K > 2 && stones.length % (K - 1) != 1)) return -1
    if(stones.length == K) return stones.sum
    (for(x <- 0 to stones.length - K) yield {
      val sum = stones.slice(x,x+K).sum
      val newStone = stones.slice(0,x) ++ Array(sum) ++ stones.slice(x+K, stones.length)
      sum + mergeStones_bf(newStone, K)
    }).min
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
          f(i)(j)(k) = f(i)(j)(k) min f(i)(t)(k-1)+f(t+1)(j)(1)
          println(s"i: $i, j: $j, k: $k, f(i,j,k):${ f(i)(j)(k)}")
        }
        f(i)(j)(1) = f(i)(j)(K) + sum(j)-(if(i>0) sum(i-1) else 0)
        println(s"i: $i, j: $j, f(i)(j)(1) : ${f(i)(j)(1) }")
        i+=1
      }
    }
    if(f(0)(N-1)(1) >= INF) -1 else f(0)(N-1)(1)
  }



  def main(args: Array[String]): Unit = {
      val s  = Array("bella","label","roller")
      //println(commonChars(s))

    val x = Array(69,39,79,78,16,6,36,97,79,27,14,31,4)
    val x2 = Array(3,2,4,1)
    val x3 = Array(6,4,9,3,1)
    println(mergeStones_dp(x3, 3))
  }
}
